---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil är processen där en fil tillfälligt skapas för att tillfälligt lagra data under programkörning. Programmerare gör detta för lagra data som behövs under aktiviteter utan att påverka huvudfilerna.

## Så här gör du:
I Elm, vi skapar inte filer direkt, eftersom det är en frontend språk. Men vi kan stimulera data lagring inom vårt program genom användning av `Dict String String`, som kan fungera som en temporär lokal fil. Nedan är ett kodexempel.

```Elm 
modul Main exposing (..)
import Dict

type alias TempFile = Dict String String

createTempFile : TempFile
createTempFile = Dict.fromList [("TempData", "Data to be temporarily stored")]

writeToFile : String -> String -> TempFile -> TempFile
writeToFile key value tempFile = Dict.insert key value tempFile

readFromFile : String -> TempFile -> Maybe String
readFromFile key tempFile = Dict.get key tempFile

main = 
    let 
        tempFile = createTempFile
        updatedFile = writeToFile "MoreTempData" "More data to store" tempFile
    in 
        Debug.log "Read from file:" (readFromFile "MoreTempData" updatedFile)
```

## Fördjupning
Historiskt sett, temporära filer används i många programmeringsspråk för att tillfälligt lagra data. I Elm, en ren funktionell språk, vi har inte direkt tillgång till filsystemet.

Alternativen till temporära filer i Elm innefattar `LocalStorage` och `SessionStorage` i webbläsarmiljön, vilket fungerar mycket lika temporära filer.

Implementationen av temporära filer i Elm är begränsad till datalagring inom programmet, då det inte kan interagera med filsystemet direkt.

## Se också
Data lagring i Elm: https://elmprogramming.com/dict.html

LocalStorage i Elm: https://package.elm-lang.org/packages/elm/browser/latest/Browser-Storage