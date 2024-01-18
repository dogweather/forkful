---
title:                "Extrahering av delsträngar"
html_title:           "Lua: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar är när man tar en del av en sträng och använder den som en separat sträng. Det är vanligt att göra detta när man behöver hantera och manipulera data eller text på ett mer specifikt sätt. Programmerare använder substrängar för att underlätta datahantering och för att skapa mer effektiva kodlösningar.

## Hur man:
Här är två exempel på hur man kan extrahera substrängar i Lua:

### Exempel 1:
```Lua
strang = "Hej, det här är en sträng!"
print(strang:sub(5, 14)) -- Output: det här är
```

I det här exemplet tilldelar vi en sträng till variabeln "strang" och sedan använder vi funktionen "sub" för att extrahera en del av strängen från position 5 till 14. Vi använder också ": " som kallas förkolon-syntax för att kalla på en objektfunktion.

### Exempel 2:
```Lua
strang = "Hej, jag heter Lua!"
print(strang:sub(-4)) -- Output: Lua!
```

I det här exemplet använder vi samma syntax och funktion, men den här gången använder vi ett negativt tal för att extrahera den sista delen av strängen. Detta är särskilt användbart när man inte vet exakt hur många tecken man behöver extrahera.

## Djupdykning:
Funktionen "sub" introducerades i Lua 5.1 och är en del av standardbiblioteket. Det finns också andra alternativ för att extrahera substrängar, som t.ex. "string.match" och "string.find". Det är viktigt att notera att funktionen "sub" räknar teckenpositioner från 1, medan de andra funktionerna räknar från 0.

För att extrahera en del av en sträng kan man använda följande syntax: string.sub(sträng, startposition, slutposition). Om slutpositionen utelämnas kommer funktionen att extrahera alla tecken från startpositionen till slutet av strängen. Om det finns en negativ slutposition kommer funktionen att räkna bakåt från slutet av strängen.

## Se även:
- Lua dokumentation om "sub":https://www.lua.org/manual/5.3/manual.html#pdf-string.sub
- W3Schools om substrängar i Lua:https://www.w3schools.com/lua/lua_strings.asp