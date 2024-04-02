---
date: 2024-01-26 03:40:56.982492-07:00
description: "Att ta bort citattecken fr\xE5n en str\xE4ng inneb\xE4r att man skalar\
  \ bort de d\xE4r dubbla eller enkla citattecken som kramar om din text. Koda g\xF6\
  r detta f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:38.026407-06:00'
model: gpt-4-0125-preview
summary: "Att ta bort citattecken fr\xE5n en str\xE4ng inneb\xE4r att man skalar bort\
  \ de d\xE4r dubbla eller enkla citattecken som kramar om din text. Koda g\xF6r detta\
  \ f\xF6r att\u2026"
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Vad & Varför?
Att ta bort citattecken från en sträng innebär att man skalar bort de där dubbla eller enkla citattecken som kramar om din text. Koda gör detta för att sanera inmatningar, för att underlätta parsing eller för att harmonisera data som kan vara inkonsekvent citerad.

## Hur man gör:
Så här sparkar du citattecknen till trottoaren i Lua:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hej, världen!"'))     -- Hej, världen!
print(remove_quotes("'Hejdå, Citattecken!'"))  -- Hejdå, Citattecken!
```

Bingo! De där citattecknen försvann som strumpor i en torktumlare.

## Fördjupning
Folk har skrubbat bort citattecken från strängar sedan språk kunde hantera text, vilket är i stort sett för alltid. I Lua gör `gsub`-funktionen den tunga lyftningen, använder mönster som en skalpell för att avlägsna citattecken. Alternativ? Visst, du kan gå på regex i språk som stöder det, eller skriva din egen loop som tuggar igenom varje tecken (gäsp, men hej, det är din tid).

Luas mönstermatchning ger dig kraften av en regex-lite-upplevelse utan att importera ett helt bibliotek. Cirkumflexet (`^`) och dollar tecknet (`$`) matchar början och slutet av strängen respektive; `%p` matchar alla skiljetecken. Efter att ha skakat av den ledande och avslutande skiljetecken fångar vi allt annat med `(.*),` och ersätter hela träffen med den fångargruppen med hjälp av `" %1"`.

Kom ihåg att Luas mönstermatchning inte är lika kraftfull som fullfjädrade regex-motorer – till exempel kan den inte räkna eller backtracka. Denna enkelhet är både en välsignelse och en förbannelse, beroende på vilka citattecken du brottas med och var de gömmer sig.

## Se även
Dyk djupare in i Luas mönstermatchning med PiL (Programming in Lua) boken: http://www.lua.org/pil/20.2.html

För ren elegans, kolla in hur andra språk gör det för jämförelse, starta med Pythons `str.strip`: https://docs.python.org/3/library/stdtypes.html#str.strip
