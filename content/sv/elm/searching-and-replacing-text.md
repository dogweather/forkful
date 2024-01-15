---
title:                "Söka och ersätta text"
html_title:           "Elm: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering för att göra stora ändringar i en fil snabbt. Det kan också vara användbart för att hitta och byta ut vissa delar av en text eller för att konvertera filer till ett annat format.

## Hur man gör det

För att söka och ersätta text i Elm, kan du använda funktionerna `String.contains` och `String.replace`. `String.contains` returnerar sant om en viss del av en text finns i en annan text. Till exempel kan vi använda det för att kontrollera om en rad innehåller ett visst ord:

```Elm
String.contains "Hello" "Hello World"
--> True
```

För att ersätta en del av en text med en annan, kan vi använda `String.replace`:

```Elm
String.replace "Hello" "Hi" "Hello World"
--> "Hi World"
```

Om du vill söka och ersätta text i en hel fil, kan du öppna filen med `File.open` och använda `String.contains` och `String.replace` på den inlästa texten. Ta en titt på exempelkoden nedan som byter ut alla förekomster av ordet "hund" med ordet "katt" i en fil med namnet "djur.txt":

```Elm
import File
import String

main : Html msg
main =
    -- Öppna filen och läsa in texten
    let
        file = File.open "djur.txt"
        contents = File.contents file
    in
    -- Byta ut hund mot katt i texten
    case contents of
        Ok text ->
            let
                modifiedText = String.replace "hund" "katt" text
            in
                -- Skriva tillbaka den modifierade texten till filen
                File.write "djur.txt" modifiedText
        Err err ->
            Html.text "Kunde inte läsa filen."
```

## Djupdykning

Det finns många sätt att söka och ersätta text i Elm, och det beror oftast på vilken typ av data du arbetar med. Om du har en mer komplex datastruktur, som en lista av strängar, kan du använda funktionen `List.map` tillsammans med `String.replace` för att söka och ersätta text i varje sträng i listan. Eller om du vill utesluta vissa delar av texten från att ersättas kan du använda en kombination av `String.contains` och `String.replace`. Det finns också olika sätt att hantera särskilda tecken i texten, såsom å, ä, ö eller versaler och gemener. Det är viktigt att göra rätt kontroller för att undvika felaktiga ändringar i din text.

## Se också

- [Elm dokumentation för `String` modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm dokumentation för `File` modulen](https://package.elm-lang.org/packages/elm/file/latest/File)
- [En guide för sökning och ersättning i Elm](https://thoughtbot.com/blog/search-and-replace-in-elm)