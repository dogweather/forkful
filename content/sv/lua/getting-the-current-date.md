---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:15:46.133138-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet innebär att programmets körmiljö frågas om det rådande kalenderdatumet. Programmerare gör detta för funktioner som behöver tidsstämplar, som logging eller att visa dagens datum till användaren.

## Hur man gör:
Kodexempel för att hämta aktuellt datum i Lua:

```lua
os.execute("date") -- Visar datum på systemets förinställda språk/format

-- Får ett tabellobjekt med datumet och tiden
local datum = os.date("*t") -- returns a table with date and time components
print("År:", datum.year)
print("Månad:", datum.month)
print("Dag:", datum.day)

-- Får en formaterad datumsträng
local datumstrang = os.date("%Y-%m-%d")
print("Aktuellt datum:", datumstrang)
```

Sample output:
```
Aktuellt datum: 2023-04-05
```

## Djupdykning
I Lua, `os.date()` är funktionen du använder för datum och tid. Funktionen har en lång historia och härstammar från C:s standardbibliotek. I Lua, `os.date("*t")` ger dig ett tabellobjekt med alla datumkomponenter, och `os.date(formatsträng)` låter dig hämta datum som en formaterad sträng. Alternativen kan vara att använda tidsstämplar med `os.time()` eller uthärda externa bibliotek för mer komplexa datumoperationer.

## Se också:
- Lua's officiella dokumentation om datum och tid: https://www.lua.org/manual/5.4/manual.html#6.9
- Lua-users wiki om datum och tid: http://lua-users.org/wiki/OsLibraryTutorial
- En guide till Lua för nybörjare: https://www.tutorialspoint.com/lua/index.htm
