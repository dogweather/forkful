---
title:    "Elm: Sjekke om en mappe eksisterer"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Hvorfor

Å sjekke om en mappe eksisterer kan være en viktig del av programmeringen din. Det kan hjelpe deg med å sikre at du ikke prøver å åpne en mappe som ikke finnes, og å håndtere eventuelle feil som kan oppstå.

# Hvordan Du Gjør Det

For å sjekke om en mappe eksisterer i Elm, kan du bruke funksjonen `File.directoryExists` og gi den en `Path` som argument. Denne funksjonen vil returnere en `Task Bool` som indikerer om mappen eksisterer eller ikke.

```Elm
import File exposing (directoryExists)
import Task

path = "/stier/minmappe"

task = directoryExists path

Task.onComplete (\result -> 
    case result of 
        Ok exists -> 
            if exists then
                "Mappen eksisterer!"
            else
                "Mappen eksisterer ikke :("
        Err _ -> 
            "Noe gikk galt"
    ) task
```

I eksempelet over bruker vi `File.directoryExists` for å sjekke om mappen med stien `/stier/minmappe` eksisterer. Deretter bruker vi `Task.onComplete` for å behandle resultatet av oppgaven og gi en passende melding basert på om mappen eksisterer eller ikke.

# Dykk Dypere

Det er viktig å merke seg at funksjonen `File.directoryExists` ikke bare sjekker om mappen eksisterer, men også om du har tilgang til å lese den. Hvis du ikke har tilgang til mappen, vil funksjonen returnere `Ok False`. Derfor kan det være lurt å også bruke `File.fileExists` for å være sikker på at du har tilgang til mappen.

En annen ting å merke seg er at Elm ikke støtter åpning og skriving til mapper direkte. Du må bruke `File.open`-funksjonen for å få tilgang til filene i mappen.

# Se Også

- [Elm File dokumentasjon](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm File eksempelkode](https://elm-lang.org/examples/files)