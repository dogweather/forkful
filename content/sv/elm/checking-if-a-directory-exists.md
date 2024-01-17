---
title:                "Kontrollera om en mapp finns"
html_title:           "Elm: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att kontrollera om en mapp existerar är en vanlig operation för programmerare. Det innebär helt enkelt att man undersöker om en viss mapp finns på en viss plats i datorsystemet. Detta är särskilt användbart när man vill undvika att skriva över en befintlig mapp eller för att kunna navigera till rätt mapp innan man utför en viss operation.

## Så här gör du:
```Elm
directoryExists : FilePath -> Task x Bool
```

För att kontrollera om en mapp finns i Elm så kan man använda funktionen `directoryExists`. Denna funktion tar en `FilePath` som parameter och returnerar sedan en `Task` som antingen innehåller `True` om mappen existerar eller `False` om den inte gör det. Här är ett exempel på hur man kan använda denna funktion för att kontrollera om en mapp med namnet "bilder" finns på skrivbordet:

```Elm
import Task exposing (..)
import File.Path exposing (..)

task : Task x Bool
task =
    directoryExists (fromString "/Users/username/Desktop/bilder")

main : Program x
main =
    task
        |> andThen (\exists -> if exists then text "Mappen finns!" else text "Mappen finns inte.")
        |> Task.perform identity
```

I det här fallet använder vi `andThen` för att hantera `Task` och skriva ut ett meddelande beroende på resultatet.

## Djupdykning
Att kontrollera om en mapp existerar är en del av filhanteringsfunktionerna i Elm. Det finns också andra sätt att hantera filer och mappar, som t.ex. att lista dem eller skapa nya. Det är också möjligt att använda JavaScript-funktioner för att utföra mer avancerade filhanteringsoperationer.

Det är viktigt att notera att `directoryExists`-funktionen använder sig av `Task` för att hantera asynkrona operationer. Detta gör att man kan skriva säkrare kod och hantera eventuella fel på ett bättre sätt.

## Se även
- Filhanteringsfunktioner i Elm: https://package.elm-lang.org/packages/elm/file/latest/
- Elm dokumentation: https://guide.elm-lang.org/
- JavaScript-funktionalitet i Elm: https://guide.elm-lang.org/interop/javascript.html