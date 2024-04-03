---
date: 2024-01-20 17:34:40.566760-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:37.821262-06:00'
model: gpt-4-1106-preview
summary: .
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## How to:
```Elm
-- String concatenation using the (++) operator
fullName : String
fullName = "Ada " ++ "Lovelace"

-- Output: "Ada Lovelace"
```

```Elm
-- Concatenating more than two strings
greeting : String
greeting = "Hej, " ++ "mitt " ++ "namn " ++ "är " ++ fullName ++ "!"

-- Output: "Hej, mitt namn är Ada Lovelace!"
```

```Elm
-- Concatenating strings with numbers
age : Int
age = 28

ageMessage : String
ageMessage = "Jag är " ++ String.fromInt(age) ++ " år gammal."

-- Output: "Jag är 28 år gammal."
```

## Deep Dive
Strängkonkatenering har alltid varit en grundläggande del av programmering. I Elm används oftast (++) operatorn för att enkelt sätta ihop strängar. Historiskt sett har olika språk olika metoder: Python använder `+`, JavaScript använder `+` eller template literals, och Haskell, liksom Elm, använder `++`. 

Det finns alternativ till (++) i Elm, såsom `String.concat` som tar en lista av strängar och sammansätter dem. I termer av prestanda kan stora strängkonkateneringar vara kostsamma, så Elm's interna implementation försöker optimera detta. Elm compilerar till JavaScript, så slutresultatet av konkatenering måste också vara effektivt i JavaScript-miljön.

## See Also
- Elm's official String documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm's `String.fromInt` function: [https://package.elm-lang.org/packages/elm/core/latest/String#fromInt](https://package.elm-lang.org/packages/elm/core/latest/String#fromInt)
- String concatenation performance in functional languages.
