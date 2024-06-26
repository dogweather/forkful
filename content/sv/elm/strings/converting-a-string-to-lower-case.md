---
date: 2024-01-20 17:38:29.235770-07:00
description: "How to: I Elm anv\xE4nder vi `String.toLower` f\xF6r att konvertera\
  \ str\xE4ngar till gemener."
lastmod: '2024-03-13T22:44:37.816381-06:00'
model: gpt-4-1106-preview
summary: "I Elm anv\xE4nder vi `String.toLower` f\xF6r att konvertera str\xE4ngar\
  \ till gemener."
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

## How to:
I Elm använder vi `String.toLower` för att konvertera strängar till gemener.

```elm
import String

-- Konvertera en sträng till gemener
lowercaseString : String -> String
lowercaseString str =
  String.toLower str

-- Exempelanvändning och utskrift
example : String
example =
  lowercaseString "Hej VÄRLDEN!"

-- Detta kommer att skriva ut "hej världen!"
```

## Deep Dive
Strängmanipulering är en grundläggande del av många programmeringsspråk, och Elm är inget undantag. Metoden `String.toLower` är inbyggd i Elms standardbibliotek och ger en enkel lösning för att konvertera strängar till gemener. Alternativt kan man manuellt iterera genom varje tecken och konvertera det, men det är onödigt komplicerat och felkänsligt. Implementeringen av `String.toLower` kan använda sig av JavaScript-funktionen `.toLowerCase()` eftersom Elm kompilerar ner till JavaScript, men detaljerna är abstractade från användaren.

## See Also
- Elm `String` paketdokumentation: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Elm språkguide: https://guide.elm-lang.org
- MDN webbdokumentation på `.toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
