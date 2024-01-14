---
title:    "Elm: Stora bokstäver i en sträng"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Varför skulle någon vilja omvandla en sträng till stor bokstav? Det kan finnas många anledningar till detta, till exempel för att göra utskrifter enhetliga eller för att se till att inmatade data är formaterade korrekt.

## Hur man gör det

Att omvandla en sträng till stor bokstav är en enkel process i Elm. Det finns en inbyggd funktion som heter `String.toUpper`, som tar en sträng som indata och returnerar samma sträng i versaler. Här är ett exempel på hur man använder den:

```elm
import String exposing (toUpper)

strang = "hej världen"
versal = toUpper sträng
```

Den första raden importerar funktionen `toUpper` från modulen `String`. Sedan skapar vi en variabel `strang` som innehåller vår sträng. Sedan skapar vi en variabel `versal` som får värdet av att tillämpa funktionen `toUpper` på `strang`. Resultatet blir "HEJ VÄRLDEN".

## Djupdykning

För att utforska detta lite närmare, låt oss titta på koden bakom `toUpper`-funktionen. I Elm är strängar bara en lista av tecken, så funktionen använder sig av den inbyggda `List.map` för att applicera funktionen `Char.toUpper` på varje enskilt tecken i strängen. Detta gör att vi kan hantera strängar av alla längder utan att behöva oroa oss för specialfall.

## Kolla även in

- [Elm dokumentation om String modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Enkel Elm tutorial för nybörjare](https://guide.elm-lang.org/)
- [Elm community forum](https://discourse.elm-lang.org/)