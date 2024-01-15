---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Elm: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Det kan være viktig å sjekke om en mappe eksisterer i en Elm applikasjon for å sikre at programmet fungerer som forventet og unngå unødvendige feil. Dette er spesielt relevant når man håndterer store mengder data eller når man trenger å koble sammen forskjellige filsystemer.

# Hvordan

For å sjekke om en mappe eksisterer i Elm trenger man først å importere nødvendige pakker. Deretter kan man bruke funksjonen `folderExists` fra pakken `elm/file` for å sjekke om mappen finnes. Her følger et eksempel på hvordan dette kan gjøres:

```Elm
import File
import System.Directory

folderExists : String -> Cmd msg
folderExists folder =
    File.folderExists folder
        |> Task.perform FolderExists

type Msg
    = FolderExists Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FolderExists exists ->
            if exists then
                -- Do something if the folder exists
            else
                -- Do something if the folder doesn't exist
```

Denne koden vil returnere en `Bool` (sant eller falskt) basert på om mappen eksisterer eller ikke. Man kan også bruke `File.fileExist` for å sjekke om en bestemt fil eksisterer i en mappe.

# Dypdykk

Det er viktig å være klar over at funksjonen `folderExists` bare sjekker om en mappe er tilgjengelig i det nåværende filsystemet. Dette betyr at hvis man jobber med et virtuelt filsystem, for eksempel i en webapplikasjon, vil funksjonen returnere `True` selv om mappen ikke eksisterer i det virkelige filsystemet. Det kan derfor være lurt å inkludere ekstra betingelser i koden for å sikre at mappen faktisk eksisterer der man ønsker.

# Se Også

- [Offisiell dokumentasjon for elm/file pakken](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm tutorial på elmprogramming.com](https://elmprogramming.com/) (på engelsk)
- [Gjør Elm koden din raskere med perfekt optimeringstips](https://medium.com/@slashdotdash/elm-perfekt-optimeringstips-872aecf6d889) (på engelsk)