---
title:    "Elm: Leser en tekstfil"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil kan være avgjørende når man jobber med programmering. Enten det er for å importere data, behandle store mengder informasjon, eller bare for å utføre en rekke handlinger basert på innholdet i filen, er det viktig å ha kunnskap om hvordan man kan lese en tekstfil i Elm.

## Hvordan

For å lese en tekstfil i Elm, kan vi bruke funksjonen `File.lines` som tar inn en `File` og returnerer en `Task` som inneholder en liste med hver linje i filen som en `String`. Her er et eksempel på hvordan man kan implementere dette:

```Elm
import File exposing (lines)
import Task exposing (Task)
import Result exposing (Result)
import String

-- Leser innholdet i en tekstfil og returnerer en liste med linjene som en Task
readFile : File -> Task String (Result String (List String))
readFile file =
   lines file

-- Funksjon for å konvertere tekstfilen til en liste med ord
fileToList : File -> Task String (Result String (List String))
fileToList file =
   Task.map (List.map String.words) (readFile file)
```

Etter å ha importert de nødvendige modulene, kan vi bruke `readFile`-funksjonen til å lese innholdet i filen. Deretter kan vi bruke `fileToList`-funksjonen til å konvertere tekstfilen til en liste med ord. Vi kan også jobbe med linjene individuelt ved å bruke funksjoner som `List.map` og `String.split` etter å ha fått tilgang til linjene som en liste.

## Dypdykk

Det er verdt å merke seg at `File.lines`-funksjonen tar en `File` som argument, som må være en fil som allerede er lastet opp til Elm-programmet ditt. For å laste opp en fil til Elm, kan vi bruke en input-fil-feltet og `File.Select`-modulen. Det er også viktig å håndtere eventuelle feil som kan oppstå under lesingen av filen ved å bruke `Result`-modulene.

## Se også

- [Elm dokumentasjon om å lese filer](https://package.elm-lang.org/packages/elm/file/latest/)
- [Eksempel på en tekstfil i Elm](https://github.com/elm/file/blob/1.0.4/examples/File.elm) 
- [Elm Guide om filer og input](https://guide.elm-lang.org/interop/file.html)