---
title:    "Elm: Läsa en textfil"
keywords: ["Elm"]
---

{{< edit_this_page >}}

##Varför

Att läsa in textfiler är en viktig färdighet i många programmeringsspråk, inklusive Elm. Genom att läsa in en textfil kan du enkelt behandla och använda informationen som den innehåller. Om du är nybörjare i Elm kan den här färdigheten hjälpa dig att bli mer bekant med språkets syntax och funktionalitet.

##Hur man gör

Det första steget är att använda "File" modulen, som tillhandahåller funktioner för att läsa in en textfil. Du kan importera modulen genom att lägga till följande i toppen av ditt Elm skript:

```Elm
import File exposing (..)
```

Vi ska nu läsa in en textfil med hjälp av File.read funktionen. Vi behöver först en filväg för textfilen som vi vill läsa in. Detta kan göras genom att lägga till filvägen som en sträng i read funktionens parameter. Till exempel:

```Elm
File.read "textfil.txt"
```

Om filen finns i samma katalog som ditt Elm skript kan du bara skriva filnamnet. Annars måste du ange hela filvägen.

För att sedan behandla informationen som lästs in från filen kan du använda en "case" sats. Till exempel:

```Elm
case File.read "textfil.txt" of
    Err error ->
        -- behandla eventuella fel

    Ok text ->
        -- behandla innehållet i textfilen
```

##Djupdykning

Det är också möjligt att läsa in en fil asynkront, vilket innebär att din kod inte behöver vänta på att filen ska läsas innan den fortsätter att köra. Detta är användbart för att undvika potentiella fördröjningar i din programkod. Det kan uppnås genom att använda funktionen File.asyncRead istället för File.read. Till exempel:

```Elm
File.asyncRead "textfil.txt" (\result ->
    case result of
        Err error ->
            -- behandla eventuella fel

        Ok text ->
            -- behandla innehållet i textfilen
    )
```

För mer information om både synkrona och asynkrona metoder för att läsa in textfiler, se "File" modulens dokumentation på Elm hemsidan.

## Se även

- [File modulens dokumentation](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Officiell Elm hemsida](https://elm-lang.org/)
- [Elm för nybörjare - En introduktion till Elm](https://elmprogramming.com/)