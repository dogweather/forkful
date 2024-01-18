---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "Lua: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster er en vanlig oppgave for programmerere. Dette kan gjøres for å fjerne uønsket tekst fra en string, eller for å implementere en form for tekstbehandling i et program.

## Hvordan:
For å slette tegn som matcher et mønster i Lua kan du bruke funksjonen `string.gsub(pattern, replacement, string)`. Patternet er det mønsteret du ønsker å matche, replacement er hva du ønsker å erstatte det matchede tegnet med, og string er den aktuelle teksten. Her er et eksempel på å slette alle bokstaver fra teksten "Hei, dette er et eksempel!" og bare beholde tallene.

```
Lua
local text = "Hei, dette er et eksempel!"
local new_text = string.gsub("[a-zA-Z]", "", text)
print(new_text) --> , 12345 !
```

## Dypdykk:
Å slette tegn som matcher et mønster er en vanlig oppgave innenfor tekstbehandling og databehandling. Det er en enkel og effektiv måte å manipulere tekst på, og brukes ofte for å filtrere ut uønskede tegn eller for å formatere teksten på en spesifikk måte. Alternativer til `gsub`-funksjonen er `string.match` og `string.gmatch`, som gir litt forskjellig funksjonalitet, men også kan brukes for å slette tegn som matcher et mønster. Implementasjonen av `gsub`-funksjonen er basert på regEx-mønstre og er inspirert av lignende funksjoner i andre programmeringsspråk.

## Se også:
Hvis du ønsker å lære mer om sletting av tegn som matcher et mønster i Lua, kan du ta en titt på Lua dokumentasjonen eller andre programmeringsressurser som dekker regEx-mønstre. Det finnes også mange nyttige verktøy og biblioteker som kan hjelpe deg med å håndtere tekstbehandling i Lua.