import lexbase, streams
from strutils import Digits
from unicode import Rune, toUTF8

const
  IdentChars = {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}
  Whitespace = {' ', '\t'}

type
  TomlTokenKind* = enum ## Type of token
    ttkEquals,          ## '='
    ttkStringLit,       ## Token is a string literal
    ttkIntegerLit,      ## Token is an integer literal
    ttkFloatLit,        ## Token is a float literal
    ttkDateTimeLit,     ## Token is a date time literal
    ttkTrue,            ## Token is boolean value 'true'
    ttkFalse,           ## Token is boolean value 'false'
    ttkIdent,           ## Token is an identifier (aka key)
    ttkBracketLe,       ## '['
    ttkBracketRi,       ## ']'
    ttkCurlyLe,         ## '{'
    ttkCurlyRi,         ## '}'
    ttkComma,           ## ','
    ttkDot,             ## '.'
    ttkInvalid,         ## Invaild token
    ttkNewLine,         ## Newline
    ttkEof              ## End-of-file

  TomlParser* {.final.} = object of BaseLexer ## Parser object
    tokenKind: TomlTokenKind                  ## Token kind
    token: string                             ## String representation of token
    filename: string                          ## Name of file being lexed

proc handleNewLines(tl: var TomlParser; pos: int): int =
  case tl.buf[pos]
  of '\c': result = tl.handleCR(pos)
  of '\L': result = tl.handleLF(pos)
  else: result = pos

proc skip(tl: var TomlParser) =
  var
    pos = tl.bufpos
    buf = tl.buf

  while true:
    case buf[pos]
    of Whitespace:
      pos.inc()
    of '#':
      while buf[pos] notin (NewLines + {EndOfFile}): pos.inc()
    else:
      break

  tl.bufpos = pos

proc handleHexChar(c: char; x: var int): bool =
  result = true # success
  case c
  of '0'..'9': x = (x shl 4) or (ord(c) - ord('0'))
  of 'a'..'f': x = (x shl 4) or (ord(c) - ord('a') + 10)
  of 'A'..'F': x = (x shl 4) or (ord(c) - ord('A') + 10)
  else: result = false # error

proc getEscapedUnicode(buf: cstring; pos: var int; len: int): int =
  for _ in 1..len:
    if handleHexChar(buf[pos], result):
      pos.inc()
    else:
      result = -1
      break

  # Escaped unicode char must be vaild Unicode scalar value
  if (result notin 0..0x7FF) and (result notin 0xE000..0x10FFFF):
    result = -1

proc handleString(tl: var TomlParser; raw: bool): TomlTokenKind =
  var
    buf = tl.buf
    pos = tl.bufpos + 1 # skip " or '

  result = ttkStringLit

  let
    strQuote = if raw: '\'' else: '"'
    multiLine = (buf[pos] == strQuote) and (buf[pos + 1] == strQuote)

  if multiLine:
    pos.inc(2)
    pos = tl.handleNewLines(pos) # handle newline (if there is any)
    buf = tl.buf

  while true:
    case buf[pos]
    of '\\':
      if raw:
        tl.token.add('\\')
        pos.inc()
      else:
        pos.inc() # skip \

        case buf[pos]
        of 'b':
          tl.token.add('\b')
          pos.inc()
        of 't':
          tl.token.add('\t')
          pos.inc()
        of 'n':
          tl.token.add('\l')
          pos.inc()
        of 'f':
          tl.token.add('\f')
          pos.inc()
        of 'r':
          tl.token.add('\r')
          pos.inc()
        of '"':
          tl.token.add('"')
          pos.inc()
        of '\\':
          tl.token.add('\\')
          pos.inc()
        of 'u', 'U':
          pos.inc()

          let uniChar = if buf[pos - 1] == 'u':
                          buf.getEscapedUnicode(pos, 4)
                        else:
                          buf.getEscapedUnicode(pos, 8)

          if uniChar < 0:
            result = ttkInvalid
            break
          else:
            tl.token.add(uniChar.Rune().toUTF8())
        of lexbase.NewLines:
          if multiLine:
            while buf[pos] in (NewLines + Whitespace):
              if buf[pos] in NewLines:
                pos = tl.handleNewLines(pos)
                buf = tl.buf
              else:
                pos.inc()
          else:
            result = ttkInvalid
            break
        else:
          result = ttkInvalid
          break
    of NewLines:
      if not multiLine:
        result = ttkInvalid
        break
      else:
        pos = tl.handleNewLines(pos)
        buf = tl.buf
        tl.token.add("\n")
    of ({'\x0'..'\x1F'} - NewLines):
      result = ttkInvalid
      break
    elif buf[pos] == strQuote:
      if multiLine and (buf[pos + 1] == strQuote) and (buf[pos + 2] == strQuote):
        pos.inc(3)
        break
      elif not multiLine:
        pos.inc()
        break

      tl.token.add(strQuote)
      pos.inc()
    else:
      tl.token.add(buf[pos])
      pos.inc()

  tl.bufpos = pos

iterator digits(buf: cstring; pos: var int): tuple[count: int; digit: char] =
  var count = 0

  while true:
    case buf[pos]
    of Digits:
      count.inc()
      yield(count, buf[pos])
    of '_':
      if buf[pos + 1] notin Digits:
        break
    else:
      break

    pos.inc()

proc handleDateTime(buf: cstring; pos: var int; token: var string): TomlTokenKind =
  result = ttkDateTimeLit

  var
    numOfDigits = 0
    numOfIter = if buf[pos] == 'T': 3 else: 2
  let
    expectChar = case buf[pos]
      of ':', '-': buf[pos]
      of 'T': ':'
      else: '\0'
    noTimeZone = buf[pos] == ':'

  if expectChar == '\0':
    result = ttkInvalid
    return
  else:
    token.add(buf[pos])
    pos.inc()

  template getNum() =
    for i in 1..numOfIter:
      for n, digit in buf.digits(pos):
        numOfDigits = n
        if numOfDigits <= 2:
          token.add(buf[pos])
        else:
          result = ttkInvalid
          return

      if numOfDigits < 2:
        result = ttkInvalid
        return

      if i == numOfIter:
        discard
      elif buf[pos] == expectChar:
        token.add(expectChar)
        pos.inc()
      else:
        result = ttkInvalid
        return

  getNum()

  if expectChar == ':':
    if buf[pos] == '.':
      token.add('.')
      pos.inc()

      for n, digit in buf.digits(pos):
        numOfDigits = n
        if numOfDigits <= 6:
          token.add(buf[pos])
        else:
          discard

      if numOfDigits < 1:
        result = ttkInvalid
        return

    if not noTimeZone:
      case buf[pos]
      of 'Z':
        token.add('Z')
        pos.inc()
      of '-', '+':
        token.add(buf[pos])
        pos.inc()

        numOfIter = 2
        getNum()
      else:
        discard
  elif (expectChar == '-') and (buf[pos] == 'T'):
    result = buf.handleDateTime(pos, token)

proc handleFloat(buf: cstring; pos: var int; token: var string): TomlTokenKind =
  result = ttkFloatLit

  var numOfDigits = 0
  case buf[pos]
  of '.':
    token.add('.')
    pos.inc()

    for n, digits in buf.digits(pos):
      numOfDigits = n
      token.add(digits)

    if numOfDigits < 1:
      result = ttkInvalid
      return

    if buf[pos] in {'e', 'E'}:
      result = buf.handleFloat(pos, token)
      return
  of 'e', 'E':
    token.add(buf[pos])
    pos.inc()

    if buf[pos] in {'+', '-'}:
      token.add(buf[pos])
      pos.inc()

    for n, digits in buf.digits(pos):
      numOfDigits = n
      token.add(digits)

    if numOfDigits < 1:
      result = ttkInvalid
      return
  else:
    result = ttkInvalid

proc handleNumericType(tl: var TomlParser): TomlTokenKind =
  var
    pos = tl.bufpos
    buf = tl.buf

  let notDateTime = buf[pos] in {'+', '-'}

  result = ttkIntegerLit

  if notDateTime:
    tl.token.add(buf[pos])
    pos.inc()

  let notNumber = (buf[pos] == '0') and (buf[pos + 1] in Digits)

  if notNumber and notDateTime:
    result = ttkInvalid
    tl.bufpos = pos
    return

  var numOfDigits = 0
  for n, digit in buf.digits(pos):
    numOfDigits = n
    if notNumber and (numOfDigits > 4):
      result = ttkInvalid
      tl.bufpos = pos
      return
    tl.token.add(digit)

  if not notDateTime:
    case buf[pos]
    of '-':
      if numOfDigits == 4:
        result = buf.handleDateTime(pos, tl.token)
        tl.bufpos = pos
        return
      else:
        result = ttkInvalid
        tl.bufpos = pos
        return
    of ':':
      if numOfDigits == 2:
        result = buf.handleDateTime(pos, tl.token)
        tl.bufpos = pos
        return
      else:
        result = ttkInvalid
        tl.bufpos = pos
        return
    elif (numOfDigits <= 4) and notNumber:
      result = ttkInvalid
      tl.bufpos = pos
      return
    else:
      discard

  if buf[pos] in {'.', 'E', 'e'}: result = buf.handleFloat(pos, tl.token)

  tl.bufpos = pos

proc handleIdent(tl: var TomlParser): TomlTokenKind =
  result = ttkIdent

  var
    pos = tl.bufpos
    buf = tl.buf

  while buf[pos] in IdentChars:
    tl.token.add(buf[pos])
    pos.inc()

  if tl.token.len() < 1:
    result = ttkInvalid

  tl.bufpos = pos

proc getToken(tl: var TomlParser; expectIdent = false): TomlTokenKind =
  tl.tokenKind = ttkInvalid
  tl.token.setLen(0)

  tl.skip()

  case tl.buf[tl.bufpos]
  of NewLines:
    tl.tokenKind = ttkNewLine
    tl.bufpos = tl.handleNewLines(tl.bufpos)
  of '=':
    tl.tokenKind = ttkEquals
    tl.bufpos.inc()
  of '[':
    tl.tokenKind = ttkBracketLe
    tl.bufpos.inc()
  of ']':
    tl.tokenKind = ttkBracketRi
    tl.bufpos.inc()
  of '{':
    tl.tokenKind = ttkCurlyLe
    tl.bufpos.inc()
  of '}':
    tl.tokenKind = ttkCurlyRi
    tl.bufpos.inc()
  of ',':
    tl.tokenKind = ttkComma
    tl.bufpos.inc()
  of '.':
    tl.tokenKind = ttkDot
    tl.bufpos.inc()
  of '"':
    tl.tokenKind = tl.handleString(false)
  of '\'':
    tl.tokenKind = tl.handleString(true)
  elif expectIdent:
    tl.tokenKind = tl.handleIdent()
  else:
    case tl.buf[tl.bufpos]
    of Digits, '+', '-':
      tl.tokenKind = tl.handleNumericType()
    of (IdentChars - Digits - {'-'}):
      tl.tokenKind = tl.handleIdent()
      case tl.token
      of "true": tl.tokenKind = ttkTrue
      of "false": tl.tokenKind = ttkFalse
      else: discard
    of EndOfFile:
      tl.tokenKind = ttkEof
    else:
      tl.tokenKind = ttkInvalid

  result = tl.tokenKind

proc open*(tl: var TomlParser; input: Stream; filename = "";
           lineOffset = 0) =
  lexbase.open(tl, input)
  tl.tokenKind = ttkInvalid
  tl.token = ""
  tl.filename = filename
  tl.linenumber.inc(lineOffset)

when isMainModule:
  import os

  if paramCount() != 1:
    stderr.writeLine("Usage: ", getAppFilename().extractFilename(), " <file>")
    quit(QUITFAILURE)

  var t: TomlParser
  let f = paramStr(1).newFileStream()

  if f.isNil():
    stderr.writeLine("Error: File '", paramStr(1), "' not found")
    quit(QUITFAILURE)

  t.open(f)

  while t.getToken() != ttkEof:
    stdout.writeLine("Token: ", t.token, " # Kind: ", t.tokenKind)

    if t.tokenKind == ttkInvalid:
      break
