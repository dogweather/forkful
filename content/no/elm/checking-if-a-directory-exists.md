---
title:                "Elm: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor 

I denne bloggposten skal vi se nærmere på hvordan man kan sjekke om en mappe eksisterer i Elm-programmeringsspråket, og hvorfor dette kan være nyttig.

## Hvordan Gjøre Det 

Det første vi må gjøre er å importere den nødvendige pakken ved hjelp av følgende kode:

```Elm
import File
```

Deretter kan vi bruke funksjonen `File.exists` for å sjekke om en mappe eksisterer. For eksempel, hvis vi ønsker å sjekke om mappen "bilder" finnes, kan koden se slik ut:

```Elm
File.exists "bilder"
```

Denne funksjonen returnerer en `Task Bool`, som betyr at den enten vil returnere `True` hvis mappen eksisterer, eller `False` hvis den ikke gjør det. Vi kan håndtere denne tasken ved hjelp av funksjonen `Task.andThen`, som lar oss fortsette å kjøre koden vår basert på resultatet av tasken.

```Elm
File.exists "bilder"
    |> Task.andThen 
        (\exists ->
            if exists then
                -- Fyll inn koden du ønsker å kjøre dersom mappen eksisterer
            else
                -- Fyll inn koden du ønsker å kjøre dersom mappen ikke eksisterer
        )
```

## Dypdykk 

Når vi bruker funksjonen `File.exists`, blir ikke mappen faktisk sjekket på nåværende tidspunkt. Dette gjøres først når tasken blir kjørt senere i koden. Dette kan være nyttig å være klar over, spesielt hvis du forventer at mappen skal eksistere når du kjører koden din.

Det er også verdt å nevne at denne funksjonen bare sjekker etter mapper i samme directory som din Elm-fil. Dersom du vil sjekke om en mappe eksisterer i et annet directory, kan du bruke funksjonen `File.exactlyExists` i stedet.

## Se Også 

- [Elm Dokumentasjon - File](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm Programmeringspråket](https://elm-lang.org/)
- [Elm Norsk Brukernettverk](https://www.meetup.com/Elm_Norge/)