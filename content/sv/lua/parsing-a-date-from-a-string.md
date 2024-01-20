---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka (parse) ett datum från en sträng innebär att konvertera en sträng som representerar ett datum till ett verkligt datumobjekt. Programmerare gör detta när de vill jämföra, sortera eller på något sätt manipulera datum i sin kod.

## Så här gör du:

I Lua kan du använda `os.time` och `os.date` för att tolka ett datum från en sträng. Låt oss se ett exempel:

```Lua
str = "2022-11-24 13:52:08"

-- tolka str datum till ett tabell objekt 
datum = {
  year = string.sub(str, 1, 4),
  month = string.sub(str, 6, 7),
  day = string.sub(str, 9, 10),
  hour = string.sub(str, 12, 13),
  min = string.sub(str, 15, 16),
  sec = string.sub(str, 18, 19)
}

-- konvertera datumet till sekunder sedan 1970
os_datum = os.time(datum)

-- skriver ut datumet 
print(os.date("%Y-%m-%d %H:%M:%S", os_datum))
```

När du kör den här koden får du följande utmatning:

```Lua
2022-11-24 13:52:08
```
Alla dessa operationer är möjliga tack vare datum/tid-funktioner som finns inbyggda i Lua.

## Djupdykning

Att tolka datum från strängar i Lua kan vara tidskrävande om du inte är bekant med strängmetoderna och datum/tid-funktionerna. Historiskt sett erbjuder andra programmeringsspråk mer stöd för denna uppgift, men i Lua tillhandahåller `os.time` och `os.date` allt som behövs för att utföra denna uppgift. 

Det finns olika sätt att tolka datum från strängar, och det finns inget definitivt "rätt" sätt. Den bästa metoden beror på vilket format dina datumsträngar kommer i och vilka krav din program har.

Om ditt datum inte är i YYYY-MM-DD HH:MM:SS-formatet, behöver du justera substringsekvenserna för att matcha ditt format och kanske till och med implementera mer komplex logik för att hantera olika format.

## Se Även

- Lua 5.3 Manualen: https://www.lua.org/manual/5.3/manual.html#6.9
- Lua-Users Wiki, Datums och Tid: http://lua-users.org/wiki/DateAndTime
- Stack Overflow, Hur man tolkar en datumsträng i Lua: https://stackoverflow.com/questions/36054967/how-to-parse-a-date-string-in-lua