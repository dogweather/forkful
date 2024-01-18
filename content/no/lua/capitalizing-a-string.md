---
title:                "Kapitalisering av en streng"
html_title:           "Lua: Kapitalisering av en streng"
simple_title:         "Kapitalisering av en streng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å "kapitalisere" en streng i programmering betyr å gjøre den første bokstaven stor og resten av bokstavene små. Dette er en vanlig konvensjon innen programmering for å gjøre koden mer lesbar og konsistent. Det gjør at variabler og funksjoner skiller seg tydelig ut fra tekst og andre elementer i koden.

## Hvordan:
Hvis du vil kapitalisere en streng i Lua, er det flere forskjellige måter å gjøre det på. Du kan enten bruke den innebygde funksjonen "string.upper" for å gjøre alle bokstavene i strengen store, eller du kan bruke "string.sub" for å utheve den første bokstaven og deretter bruke "string.upper" på den. 

```Lua
-- Eksempel på bruk av string.upper funksjonen:
local tekst = "hei, dette er en test"
print(string.upper(tekst))
-- Output: HEI, DETTE ER EN TEST

-- Eksempel på bruk av string.sub og string.upper:
local tekst = "hei, dette er en test"
local uthevetTekst = string.sub(tekst, 1, 1) -- Henter ut den første bokstaven
local restenAvTekst = string.sub(tekst, 2) -- Henter ut resten av teksten
print(string.upper(uthevetTekst) .. restenAvTekst)
-- Output: Hei, dette er en test
```

## Dypdykk:
Å kapitalisere tekst er en konvensjon som har vært brukt i programmering i lang tid. Det hjelper til med å gjøre koden mer standardisert og lesbar for andre utviklere. Alternativet til å kapitalisere en streng er å bruke camel-case, hvor starten på hvert ord er stor bokstav. Dette kan være nyttig for lengre variabelnavn, men kan være mindre ønskelig for enkle og korte variabelnavn.

Det er også viktig å merke seg at Lua er et språk som er sensitivt for store og små bokstaver, så det kan være lurt å være konsekvent i hvordan du bruker kapitalisering i koden din. For eksempel, hvis du har definert en variabel "navn" med små bokstaver og prøver å bruke den som "Navn", vil Lua oppfatte dette som to forskjellige variabler.

## Se også:
- [Lua offisiell dokumentasjon om strenger](https://www.lua.org/pil/20.1.html)
- [Wikipedia artikkel om Lower Camel Case](https://en.wikipedia.org/wiki/Camel_case#Lower_camel_case)