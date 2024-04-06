---
date: 2024-01-20 17:57:45.540465-07:00
description: "Hur g\xF6r man: Sample Output."
lastmod: '2024-04-05T21:53:39.148694-06:00'
model: gpt-4-1106-preview
summary: ''
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## Hur gör man:
```Elm
import String

-- Funktion för att söka och ersätta text
replaceText : String -> String -> String -> String
replaceText searchText replaceWith fullText =
    String.Extra.replaceAll searchText replaceWith fullText

-- Användningsfall: Ersätt 'äpple' med 'päron'
exampleOutput : String
exampleOutput =
    replaceText "äpple" "päron" "Jag gillar att äta äpple!"

-- Testa funktionen
main =
    Html.text exampleOutput
```

Sample Output:
```Elm
"Jag gillar att äta päron!"
```

## Fördjupning
Historiskt sett har behovet av att söka och ersätta text funnits sedan de första texteditorerna skapades. I Elm, String-modulets funktioner erbjuder enkla sätt att hantera strängar. Det finns också paket som `elm-community/string-extra` för mer avancerade operationer. En annan metod är att använda regular expressions, men Elm har inget inbyggt stöd för dem ännu. Utförandet är ofta effektivt genom att delar av strängen inte kopieras omöjligt, vilket sparar minne.

## Se även
- Elm String documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- `elm-community/string-extra` package: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
- Regular expressions guide: https://www.regular-expressions.info/
