---
title:                "Elm: Å sjekke om en mappe eksisterer"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av å skrive pålitelige og brukervennlige programmer. Ved å sjekke om en mappe eksisterer, kan du unngå feil og sikre at programmet fungerer som det skal.

## Hvordan

For å sjekke om en mappe eksisterer i Elm, kan du bruke funksjonen `Dir.exists`. Denne funksjonen tar inn en streng som representerer mappen du ønsker å sjekke, og returnerer en `Task` som enten vil returnere `True` eller `False`, avhengig av om mappen eksisterer eller ikke. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Elm
import Dir

myDirectory : String
myDirectory = "minMappe"

Dir.exists myDirectory
    |> Task.attempt handleExists

handleExists : Bool -> msg
handleExists exists =
    if exists then
        -- gjør noe dersom mappen eksisterer
    else
        -- gjør noe dersom mappen ikke eksisterer
```

I dette eksempelet oppretter vi en streng som representerer mappen vår, og deretter bruker vi funksjonen `Dir.exists` for å sjekke om denne mappen eksisterer. Avhengig av resultatet, kan vi utføre forskjellige handlinger ved hjelp av funksjonen `handleExists`.

## Dypdykk

Når du bruker funksjonen `Dir.exists`, er det viktig å merke seg at den ikke sjekker om mappen er tom eller ikke. Den sjekker bare om mappen eksisterer eller ikke. Du kan også bruke funksjonen `Dir.list` for å få en liste over filer og mapper i en spesifikk mappe.

For å gjøre dypdykket enda dypere, kan du utforske [Elm's Directory-modul](https://package.elm-lang.org/packages/elm/core/latest/Dir) for flere nyttige funksjoner for å håndtere mapper og filer i Elm.

## Se også

- [Elm Directory-modulen](https://package.elm-lang.org/packages/elm/core/latest/Dir)
- [Elm's Task-modul](https://package.elm-lang.org/packages/elm/core/latest/Task)