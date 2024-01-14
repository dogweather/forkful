---
title:                "Elm: Oppretting av en midlertidig fil"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lage midlertidige filer kan være en avgjørende del av å bygge pålitelige programmer. Med Elm, et funksjonelt programmeringsspråk, kan du enkelt lage midlertidige filer for å organisere og styre dataene dine.

# Hvordan

La oss si at du har et program som lar brukerne lage og organisere oppgaver. Du vil kanskje lagre disse oppgavene midlertidig mens brukeren jobber med dem. Her er en enkel måte å gjøre det på i Elm:

```elm
import File
import Task

taskFile : File
taskFile =
    File.temp "tasks.txt"

writeTask : String -> Task String ()
writeTask task =
    Task.succeed task
        |> Task.andThen (\t -> File.write task taskFile)

```

Når du kaller `writeTask`-funksjonen, vil den legge til den nye oppgaven i `tasks.txt`-filen. Dette vil fortsette å oppdatere filen hver gang brukeren legger til en ny oppgave. Du kan også bruke `File.read`-funksjonen for å lese data fra den midlertidige filen.

# Dypdykk

En interessant ting å merke seg er at midlertidige filer opprettes i en virtuell minneblokk og ikke faktisk på harddisken. Dette betyr at når programmet ditt avsluttes, forsvinner den midlertidige filen også. Dette gjør det til en sikker måte å lagre og håndtere midlertidige data på.

# Se også

- [Elm dokumentasjon om å lage midlertidige filer](https://package.elm-lang.org/packages/elm/file/latest/File#temp)
- [Diskusjon om midlertidige filer i Elm](https://discourse.elm-lang.org/t/temporary-files/640)