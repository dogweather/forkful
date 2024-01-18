---
title:                "Søke og erstatte tekst"
html_title:           "Lua: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstatting av tekst er en vanlig oppgave for programmerere. Det innebærer å finne et bestemt stykke tekst i en større streng og erstatte det med et annet stykke tekst. Dette kan være nyttig når man trenger å endre en bestemt del av en kode eller tekst, og gjør det enklere og mer effektivt å gjøre større endringer.

## Slik gjør du det:
For å søke og erstatte tekst i Lua, kan vi bruke funksjonen string.gsub(). Det første argumentet i denne funksjonen er strengen du vil gjøre endringer i, det andre argumentet er teksten du vil søke etter og det tredje argumentet er teksten du vil erstatte det med.

```Lua
-- Eksempel:
local tekst = "Hei, mitt navn er Lua."
local ny_tekst = string.gsub(tekst, "Lua", "Nora")
print(ny_tekst) -- Output: "Hei, mitt navn er Nora."
```

## Dykk dypere:
Søking og erstatting av tekst er en vanlig oppgave i mange programmeringsspråk. I Lua er string.gsub() en av de mest effektive måtene å gjøre dette på, men det finnes også andre metoder som kan brukes, som for eksempel string.find() og string.match(). Det som skiller disse metodene er hvor nøyaktig søket etter tekst er, og hvordan resultatet blir behandlet.

## Se også:
Lær mer om string.gsub() og andre nyttige funksjoner i Lua her: https://www.lua.org/manual/5.3/manual.html#6.4

Les mer om søking og erstatting av tekst i programmering her: https://www.programiz.com/c-programming/library-function/string/strspn