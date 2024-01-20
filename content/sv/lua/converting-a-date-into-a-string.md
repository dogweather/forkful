---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng innebär att ändra datumets format till en läsbar textsträng. Programmerare gör detta för att göra datumdata mer tillgängligt och förståeligt för användare.

## Hur gör man? 

Här är ett exempel på hur man konverterar ett datum till en sträng i Lua:

```Lua
os.setlocale('sv_SE')

-- Hämta dagens datum
local nu = os.date('*t')

-- Konvertera det till en sträng
local datumStrang = os.date('%Y-%m-%d (%A) klockan %H:%M:%S', os.time(nu))

-- Skriv ut det konverterade datumet
print(datumStrang)
```

Detta kommer att ge resultatet:

```
2022-04-11 (Måndag) klockan 13:45:30
```

## Djupdykning

- Historisk Sammanhang: Lua släpptes för första gången 1993 och har sedan dess byggt upp en robust uppsättning inbyggda funktioner för datum och tidshantering, inklusive `os.date` och `os.time` som används här.
- Alternativ: Det finns andra sätt att konvertera ett datum till en sträng i Lua. Du kan till exempel bygga din egen funktion om du behöver mycket specifikt format eller komplexitet.
- Implementeringsdetaljer: `os.date`-funktionen har två format, en som returnerar en sträng enligt ett specifikt format (om en formatsträng tillhandahålls), och en som returnerar en tabell med datuminfo om '*' tecknet används.

## Se Också

För mer information, se följande externa resurser:

- [Lua: os.date - Manual](https://www.lua.org/manual/5.2/manual.html#6.22)
- [Lua: os.time - Manual](https://www.lua.org/manual/5.2/manual.html#6.23)
- [Converting Date and Time to string in Lua](https://stackoverflow.com/questions/19664666/converting-date-and-time-to-string-in-lua) på StackOverflow