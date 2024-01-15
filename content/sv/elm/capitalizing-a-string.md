---
title:                "Stora bokstäver i en sträng"
html_title:           "Elm: Stora bokstäver i en sträng"
simple_title:         "Stora bokstäver i en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att förstora bokstäver i en sträng är en viktig funktion i många programmeringsspråk, inklusive Elm. Genom att förstå hur man gör det kan du enkelt modifiera strängar för att passa dina behov.

## Så här gör du

```elm
import String

String.toUpper "hej världen"
```

Detta kodblock använder den inbyggda funktionen `toUpper` i `String`-modulen för att förstora alla bokstäver i strängen `"hej världen"`. Resultatet är `"HEJ VÄRLDEN"`.

En annan möjlighet är att använda `String.map` för att iterera över varje tecken i en sträng och förstora det.

```elm
import String exposing (map, toUpper)

map toUpper "hej världen"
```

Detta kodblock ger samma resultat som det föregående, men visar hur man kan använda `map` för att förstora en sträng på ett mer flexibelt sätt.

## Djupdykning

Att förstora bokstäver i en sträng är en relativt enkel uppgift, men kan vara till stor hjälp i vissa situationer. Till exempel kan du använda det för att standardisera inmatning från användare eller för att enkelt jämföra strängar oavsett storlek på bokstäver.

I Elm finns det också möjlighet att förstora specifika tecken i en sträng baserat på deras position. Detta kan göras med hjälp av den inbyggda funktionen `toUpperAt`.

```elm
import String

String.toUpperAt 2 "hej världen"
```

Detta kodblock kommer att förstora bokstäverna vid position 2 i strängen, vilket resulterar i `"HeJ Världen"`.

## Se även

- [Officiell Elm-dokumentation för String-modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Guide: Strings](https://guide.elm-lang.org/types/strings.html)
- [Elm Tutorial: Working with Strings](https://korban.net/elm/elm-tutorial-working-with-strings/)