---
title:                "Elm: Omvandling av en sträng till gemener"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig uppgift när man programmerar. Det kan vara användbart för att jämföra olika strängar utan att ta hänsyn till stor/🅼/uppläggnings-bokstäver, eller för att få enhetliga utskrifter.

## Hur man gör det

```Elm
-- Definiera en funktion som konverterar en sträng till små bokstäver
toLower : String -> String
toLower str =
  -- Använd funktionen String.toLower för att konvertera varje bokstav
  String.toLower str

-- Anropa funktionen på en sträng
toLower "ELM"
-- Output: "elm"
```

## Djupdykning

När man tittar närmare på funktionen `toLower` ser man att den använder sig av `String.toLower` för att konvertera varje bokstav i strängen. Men hur fungerar det egentligen?

Elm har en inbyggd funktion som heter `String.toLower` som tar emot en `String` och returnerar en ny `String` med alla bokstäver omvandlade till små bokstäver. Detta gör den genom att använda använda sig av Unicode-tabellen för att veta vilka bokstäver som ska konverteras.

## Se även

- [Elm Language Guide](https://guide.elm-lang.org/) (på engelska)
- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String) (på engelska)
- [Unicode Character Database](https://unicode.org/Public/UCD/latest/ucd/) (på engelska)