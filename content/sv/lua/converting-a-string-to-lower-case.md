---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att omvandla en sträng till gemener innebär att ändra alla bokstäver i en sträng till små bokstäver. Programmerare gör detta för att standardisera datainput, vilket underlättar jämförelser och sökningar.

## Hur Man Gör:
Lua erbjuder en inbyggd funktion `string.lower()` för att konvertera alla bokstäver i en sträng till gemener. Här är hur det fungerar:

```Lua
str = "Hej Världen!"
result = string.lower(str)

-- Skriv ut resultatet
print(result) -- hej världen!
```

## Djup Dykning
Historisk sett har behovet av att konvertera strängar till gemener varit vanligt inom datavetenskap för att underlätta dataanalys. Alternativen till Lua's inbyggda funktion innefattar att manuellt iterera över varje tecken i strängen och konvertera det med hjälp av ASCII-kodtabellen. Mängden minne som krävs för sådana operationer och effektiviteten av din implementation kan skilja sig avsevärt baserat på storleken på din data och det specifika programmeringsspråk du använder.

## Se också
Några användbara länkar för att lära mer om strängmanipulation i Lua:

- [Mapping of Lua's string functions in Swedish](https://www.lua.org/manual/latest/)
- [Lua String Library at Tutorialspoint](https://www.tutorialspoint.com/lua/lua_strings.htm)
  
Och slutligen är [Lua Users' Wiki](http://lua-users.org/wiki/) en förtroende resurs där du kan diskutera och lära av gemenskapen.