---
title:    "Elm: Läsa en textfil"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa och hantera textfiler är en viktig del av många programmerares arbete, oavsett om det är att läsa in data från en fil eller att skapa en rapport eller loggfil. I denna bloggpost kommer vi att titta närmare på hur man kan göra detta i Elm-programmeringsspråket.

## Så här gör du

För att läsa en textfil i Elm använder vi modulen `Text.File`, som ger oss funktioner för att hantera in- och utläsning av filer. Vi börjar med att importera modulen och sedan öppna filen som vi vill läsa:

```elm
import Text.File exposing (readFile)

main =
    readFile "textfil.txt"
```

Detta ger oss tillgång till filinnehållet som en `Task`, som vi kan kedja till en `Cmd`-funktion för att utföra handlingar baserat på filens innehåll:

```elm
import Text.File exposing (readFile)

main =
    readFile "textfil.txt"
        |> Task.map doSomething
        |> Task.attempt HandleResult
 
doSomething : String -> String
doSomething content =
    -- här kan vi utföra önskade handlingar baserat på innehållet i filen
    "Det här är vad som står i filen: " ++ content
 
type Msg
    = HandleResult (Result String String)
 
update msg model =
    case msg of
        HandleResult (Ok result) ->
            -- gör något med resultatet från filen
            model
 
        HandleResult (Err error) ->
            -- hantera eventuella fel som kan ha inträffat
            model
```

## Djupdykning

För att fördjupa oss lite mer i ämnet kan det också vara intressant att veta hur man läser specifika delar av en textfil. I Elm kan vi använda funktionen `slice` från modulen `String` för att läsa ett visst antal tecken, rader eller ord från en textsträng. Till exempel:

```elm
content =
    "Detta är en textfil som innehåller några rader med text."

String.slice 0 10 content -- ger "Detta är e"
String.slice 0 1 content -- ger "D"
String.words (String.slice 0 21 content) -- ger ["Detta", "är", "en"]
```

Vi kan också använda funktionen `lines` från samma modul för att dela upp en textfil i rader och sedan göra mer specifika manipulationer baserat på dessa.

## Se även

- Officiell dokumentation för Text.File-modulen: https://package.elm-lang.org/packages/elm/file/latest/Text-File

- Elm-exempel på hantering av textfiler: https://github.com/rofrol/elm-text-file-example

- En annan bra guide för att läsa textfiler i Elm: https://www.elmbark.com/2017/11/13/reading-writing-files-in-elm/