---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att jämföra två datum innebär att avgöra vilket som kommer före eller efter, eller om de är samma datum. Detta gör programmerare när det är nödvändigt att räkna ut tid mellan händelser eller att sortera händelser i kronologisk ordning.

## Så här gör du:
I Lua använder vi os.time-funktionen för att konvertera ett datumobjekt till ett tidsstämpel, vilket gör det enkelt att jämföra. Här är ett exempel:

```Lua
--- Definiera två datum
date1 = os.time{year=2022, month=12, day=31}
date2 = os.time{year=2021, month=12, day=31}

--- Jämför datumen
if(date1 > date2) then
  print("Date1 är senare än Date2")
else
  print("Date2 är senare än Date1")
end
```

När du kör koden ovan kommer output att bli "Date1 är senare än Date2".

## Djupdykning
Tidigare i historien utgjorde hantering av datum och tid ett stort problem för programmerare på grund av dess komplexitet och inkonsekvens över olika system. 

Men Lua löser detta problem genom inbyggda datum- och tidsfunktioner som os.date och os.time. Man kan även använda andra bibliotek, till exempel LuaDate, för ännu fler funktioner.

När man jämför datum i Lua, är det viktigt att notera att os.time skapar en tidstämpel som representerar antalet sekunder som gått sedan 1970, vilket i praktiken gör datumet oberoende av tidzoner och sommartid.

## Se även
För mer information om hur du hanterar datum och tid i Lua, kan följande resurser vara användbara:
1. [Lua-Users Wiki: Dates and Time](http://lua-users.org/wiki/DatesAndTime) – Djupgående information om datum och tid i Lua.
2. [Programming in Lua : 22.1](https://www.lua.org/pil/22.1.html) – Det officiella avsnittet för datum och tidshantering i Lua.
3. [Lua os Library](https://www.tutorialspoint.com/lua/lua_os_library.htm) – En sammanfattning av Luas os-bibliotek, inklusive datum och tid.