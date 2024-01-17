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

## Hva & Hvorfor?

Det å sjekke om en mappe eksisterer er en vanlig oppgave for programmerere. Det er en måte å kontrollere og håndtere filsystemer på, og det kan også være nyttig for å sikre at programmet fungerer som forventet.

## Hvordan:

```elm
-- Sjekke om en mappe eksisterer:
 

Elm.Directory.exists "/dokumenter" 
-- Returnerer True hvis mappen eksisterer 
```

```elm
-- Sjekke om en mappe ikke eksisterer: 

Elm.Directory.notExists "/bilder" 
-- Returnerer False hvis mappen eksisterer 
```

## Dypdykk:

**Historisk kontekst:** Sjekking av mapper har vært en viktig del av programmering i lang tid, spesielt med filbehandlingssystemer.

**Alternativer:** I tillegg til Elm har også andre programmeringsspråk og plattformer funksjoner for å sjekke eksistensen av mapper. Dette inkluderer Java, Python og .NET.

**Implementasjonsdetaljer:** Elm bruker en funksjon som heter "directoryExists" for å sjekke om en mappe eksisterer. Denne funksjonen sammenlignes med en liste over eksisterende mapper på datamaskinen for å bestemme om det er en kamp.

## Se også:

- Elm Directory Module: https://package.elm-lang.org/packages/elm/core/latest/Directory
- Python os module: https://docs.python.org/3/library/os.html