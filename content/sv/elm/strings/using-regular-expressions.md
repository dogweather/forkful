---
title:                "Att använda reguljära uttryck"
aliases:
- /sv/elm/using-regular-expressions/
date:                  2024-02-03T19:16:41.169308-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda reguljära uttryck"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck (regex) inom programmering är mönster som används för att matcha teckenkombinationer i strängar. I Elm, precis som i andra språk, använder programmerare regex för uppgifter som att validera inmatning, söka och ersätta text inom strängar på grund av deras flexibilitet och effektivitet.

## Hur man gör:
Elm har inte inbyggda regex-funktioner i sitt kärnbibliotek, vilket kräver användning av tredjepartsbibliotek för dessa operationer. Ett av de populära valen för att arbeta med regex är `elm/regex`. Du kan lägga till det i ditt projekt genom att använda `elm install elm/regex`.

Så här kan du använda `elm/regex` för några vanliga uppgifter:

### 1. Matcha ett mönster
För att kontrollera om en sträng matchar ett mönster kan du använda `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Exempelanvändning:
isAlphanumeric "Elm2023"     -- Utdata: True
isAlphanumeric "Elm 2023!"   -- Utdata: False
```

### 2. Hitta alla matchningar
För att hitta alla förekomster av ett mönster inom en sträng kan du använda `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Exempelanvändning:
getWords "Elm is fun!"  -- Utdata: ["Elm", "is", "fun"]
```

### 3. Ersätta text
För att ersätta delar av en sträng som matchar ett mönster använder du `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Exempelanvändning:
replaceElmWithHaskell "Learning Elm is fun!"  
-- Utdata: "Learning Haskell is fun!"
```

I dessa exempel används `Regex.fromString` för att kompilera ett regex-mönster, där `\b` matchar ordgränser, och `\w` matchar vilket ordtecken som helst. Hantera alltid `Maybe`-resultatet av `Regex.fromString` för att skydda mot ogiltiga regex-mönster, vanligtvis med `Maybe.withDefault`.
