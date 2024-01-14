---
title:    "Elm: Sammanslagning av strängar"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en vanlig uppgift när man programmerar, oavsett vilket språk man använder. I Elm gör man detta genom att använda funktionen `String.append`. Det finns olika anledningar till varför man skulle vilja sammanslå strängar, till exempel för att skapa dynamiska meddelanden eller skriva ut data från en databas.

## Hur man gör det

Att sammanslå strängar i Elm är enkelt. Man använder helt enkelt funktionen `String.append` och anger strängarna som ska sammanslås som argument. Om man till exempel vill sammanslå strängarna "Hej" och "världen" kan man göra det på följande sätt:

```Elm
String.append "Hej" "världen"  -- Ger outputen "Hej världen"
```

Om man vill sammanslå fler än två strängar kan man använda funktionen flera gånger. Det är viktigt att notera att ordningen på argumenten påverkar outputen. Om man först anger strängen "världen" och sedan "Hej" kommer outputen att bli "världen Hej".

## Fördjupning

Det finns flera olika sätt att sammanslå strängar i Elm, beroende på vilka behov man har. Förutom funktionen `String.append` finns också `String.join`, som sammanslår en lista av strängar och lägger till ett valfritt tecken mellan varje sträng. Det finns också `String.concat`, som sammanslår en lista av strängar utan att lägga till något extra tecken.

En annan intressant funktion är `String.concatMap`, som tar en funktion som argument och sammanslår strängar baserat på resultaten från denna funktion. Detta kan vara användbart om man vill manipulera strängarna på något sätt innan de sammanslås.

Det är också möjligt att sammanslå andra datatyper, som nummer eller booleska värden, och konvertera dem till strängar innan de sammanslås.

## Se även

- [Elm dokumentation: String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Sammanslå strängar i Elm](https://guide.elm-lang.org/error_handling/results.html#sammansla-obj/dict)