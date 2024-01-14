---
title:                "Elm: Borttagning av tecken som matchar ett mönster"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför: 

I den här bloggposten kommer vi att utforska hur man kan ta bort tecken som matchar ett visst mönster i Elm-programmeringsspråket. Detta kan vara användbart när man behöver rensa eller formatera textsträngar i en applikation.

## Hur man gör: 

För att ta bort tecken som matchar ett mönster i Elm kan man använda funktionen `String.filter` tillsammans med `String.contains` för att avgöra vilka tecken som ska tas bort. Låt oss säga att vi har en textsträng som innehåller bokstäver, nummer och specialtecken och vi bara vill behålla bokstäverna. Vi kan använda följande kod för att ta bort alla tecken som inte är bokstäver:

```Elm
import String exposing (filter, contains)

strang = "Hej 123 världen!"
bokstaver = "abcdef...xyz"

renStrang = String.filter (\c -> String.contains c bokstaver) strang

-- renStrang blir nu "Hej världen"
```

I detta exempel använder vi en lambda-funktion för att kontrollera om ett tecken finns i vårt `bokstaver`-tecken. Detta gör att vi bara tar bort tecken som inte finns i vår lista med bokstäver.

## Djupdykning: 

För att göra detta ännu mer användbart kan vi skapa en funktion som tar emot en sträng och ett mönster som argument och returnerar en sträng där alla tecken som matchar mönstret tas bort. Vi kan även göra vår funktion mer flexibel genom att låta användaren välja att ta bort eller behålla tecken som matchar mönstret.

```Elm
import String exposing (filter, contains)

removeCharacters : String -> String -> Bool -> String
removeCharacters str pattern removeMatch =
    if removeMatch
        then
            String.filter (\c -> not (String.contains c pattern)) str
        else
            String.filter (\c -> String.contains c pattern) str

strang = "Hej 123 världen!"
mönster = "123"

renStrang = removeCharacters strang mönster True

-- renStrang blir nu "Hej världen"
```

I detta exempel kan vi använda vår funktion för att ta bort alla siffror från vår sträng genom att sätta `removeMatch` till `True`. Om vi vill behålla siffrorna kan vi sätta `removeMatch` till `False` istället.

## Se även: 

- [Elm String Library](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm String Docs](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [Removing Non-Numeric Characters from a String in Elm](https://medium.com/@gaurav5430/removing-non-numeric-characters-from-a-string-in-elm-72950a8ac23a)