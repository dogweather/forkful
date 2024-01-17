---
title:                "Interpolerande en sträng"
html_title:           "Haskell: Interpolerande en sträng"
simple_title:         "Interpolerande en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolering av strängar är när man skapar en ny sträng genom att sätta ihop flera mindre strängar. Detta är användbart för att dynamiskt skapa strängar med varierande innehåll, såsom att bygga en URL med olika parametrar eller att skapa en loggmeddelande med variabel data.

## Så här:

```Haskell
-- Skapa en URL med parametrar
let userId = "123"
let endpoint = "users"
let url = userId ++ "/" ++ endpoint
-- Output: "123/users"

-- Skapa en loggmeddelande
let name = "John"
let action = "logged in"
let message = name ++ " " ++ action ++ " at " ++ getCurrentTime()
-- Output: "John logged in at 2020-10-20 12:00:00"
```

## Djupdykning:

Interpolering av strängar har funnits länge och är vanligt i många programmeringsspråk, inklusive Haskell. En alternativ metod är att använda "string formatting" vilket gör att variabler kan sättas in i en sträng genom att använda speciella uttryck. I Haskell använder man istället ordningens funktioner, såsom ++, för att interpolera strängar.

## Se också:

- [https://www.haskell.org/](https://www.haskell.org/) för mer info om Haskell.
- [https://github.com/JuliaString/Interpolations.jl](https://github.com/JuliaString/Interpolations.jl) för ett exempel på interpolering i Julia.