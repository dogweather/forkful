---
title:                "Clojure: Å skrive en tekstfil"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor 

Skal man skrive tekstfiler i Clojure? Når man skriver kode i Clojure, er det ofte nødvendig å lagre data i en ekstern fil. Dette kan være for å lagre brukerinput, tekstfiler eller til og med konfigurasjonsfiler. Ved å lære hvordan man skriver til tekstfiler, kan man utvide mulighetene til sin Clojure programmering.

## Slik gjør du det 

Det er enkelt å skrive til tekstfiler i Clojure. Først må du importere `clojure.java.io` biblioteket ved å bruke `(require '[clojure.java.io :as io])`. Deretter kan du bruke funksjonen `spit` til å skrive til en fil. Her er et eksempel på hvordan du kan bruke `spit` for å skrive en enkel tekstfil:

```Clojure
(spit "tekstfil.txt" "Dette er en tekstfil laget med Clojure")
```

Nå kan du åpne filen "tekstfil.txt" og se at teksten er blitt lagt til i filen. Du kan også skrive til en eksisterende fil ved å bruke `append` funksjonen. Her er et eksempel på hvordan du kan bruke `append` for å legge til mer tekst i samme tekstfil:

```Clojure
(append "tekstfil.txt" " Dette er mer tekst som er lagt til")
```

Nå vil teksten "Dette er mer tekst som er lagt til" bli lagt til på en ny linje i filen "tekstfil.txt".

## Dykk dypere

I tillegg til å skrive til tekstfiler, kan Clojure også lese innholdet fra en fil ved hjelp av funksjonen `slurp`. Denne funksjonen vil returnere en streng med innholdet fra filen. Her er et eksempel på hvordan du kan bruke `slurp` for å lese innholdet fra filen "tekstfil.txt":

```Clojure
(slurp "tekstfil.txt")
```

Du kan også bruke `io` biblioteket til å utføre mer avanserte operasjoner, som for eksempel å lese og skrive til binære filer.

## Se også

- [clojure.java.io dokumentasjon](https://clojuredocs.org/clojure.java.io)
- [Clojure for the Brave and True](https://www.braveclojure.com/io/)
- [Offisiell Clojure hjemmeside](https://clojure.org/)