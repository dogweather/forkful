---
date: 2024-01-20 17:38:29.235770-07:00
description: "Att konvertera en str\xE4ng till gemener inneb\xE4r att omvandla alla\
  \ bokst\xE4ver i str\xE4ngen till sm\xE5 bokst\xE4ver. Programmerare anv\xE4nder\
  \ det f\xF6r enhetlighet i\u2026"
lastmod: '2024-02-25T18:49:36.111369-07:00'
model: gpt-4-1106-preview
summary: "Att konvertera en str\xE4ng till gemener inneb\xE4r att omvandla alla bokst\xE4\
  ver i str\xE4ngen till sm\xE5 bokst\xE4ver. Programmerare anv\xE4nder det f\xF6\
  r enhetlighet i\u2026"
title: "Konvertera en str\xE4ng till gemener"
---

{{< edit_this_page >}}

## What & Why?
Att konvertera en sträng till gemener innebär att omvandla alla bokstäver i strängen till små bokstäver. Programmerare använder det för enhetlighet i textbehandling, sökningar eller för att standardisera användarinput.

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
