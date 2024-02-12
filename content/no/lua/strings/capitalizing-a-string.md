---
title:                "Sette stor bokstav i en streng"
aliases:
- /no/lua/capitalizing-a-string/
date:                  2024-02-03T19:05:49.991882-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sette stor bokstav i en streng"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng innebærer å endre det første tegnet i hvert ord i en setning til stor bokstav, samtidig som resten sikres å være små bokstaver. Denne teknikken brukes ofte for å formatere tekst til et mer profesjonelt eller leselig resultat, slik som forberedelse av titler eller brukerinndata for visning.

## Hvordan:
Lua har ikke en innebygd funksjon for å kapitalisere strenger, men du kan enkelt utføre denne oppgaven ved hjelp av grunnleggende strengmanipuleringsfunksjoner. Her er en enkel funksjon for å kapitalisere det første bokstaven i et enkelt ord:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Output: Hello
```

For å kapitalisere hvert ord i en setning, kan du dele setningen inn i ord, kapitalisere hvert enkelt, og deretter slå dem sammen igjen:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Output: Hello World From Lua
```

Hvis du jobber med et prosjekt der ytelse er nøkkelen, og du finner deg selv i behov av mer avanserte strengmanipuleringskapasiteter, vurder å bruke et tredjepartsbibliotek som `Penlight`. Penlight forbedrer Lua med mer fleksible strengbehandlingsfunksjoner, blant annet:

```lua
-- Med forutsetning om at Penlight er installert:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Output: Hello lua users

-- Merk: Penlights capitalized-funksjon kapitaliserer kun det første ordet.
-- For å kapitalisere hvert ord, ville du fortsatt implementere en egendefinert løsning eller utforske andre biblioteker.
```
