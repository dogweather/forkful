---
title:    "Clojure: Sjekke om en mappe eksisterer"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger, i utviklingen av et Clojure program, kan det være viktig å vite om en bestemt mappe eksisterer eller ikke. Dette kan være nyttig for å unngå feil og for å håndtere ulike scenarioer.

## Hvordan
For å sjekke om en mappe eksisterer i Clojure, kan du bruke funksjonen `fs/exists?`. Denne funksjonen tar inn en streng som representerer banen til mappen du ønsker å sjekke. For eksempel:

```Clojure
(fs/exists? "/home/bruker/dokumenter")
```
Dette vil returnere `true` dersom mappen eksisterer, og `false` dersom den ikke eksisterer.

Du kan også sjekke om en mappe eksisterer relativt til din nåværende arbeidsmappe. Dette kan gjøres ved å bruke funksjonen `fs/exists?` sammen med `io/resource`. For eksempel:

```Clojure
(fs/exists? (io/resource "bilder/logg.png"))
```

Dette vil returnere `true` eller `false` avhengig av om mappen eksisterer eller ikke.

## Dypdykk
Når du bruker `fs/exists?`, bruker Clojure faktisk Java-koen `java.io.File` for å sjekke mappen. Dette betyr at dersom du har flere operativsystemer som utviklingsmiljø (for eksempel Linux og Windows), kan du oppleve forskjellig oppførsel når du bruker `fs/exists?`.

Dette skyldes forskjeller i hvordan disse operativsystemene representerer baner. For eksempel vil Windows bruke en `\` for å skille mapper i en bane, mens Linux vil bruke `/`.

Det er også viktig å merke seg at `fs/exists?` kun sjekker om en mappe eksisterer, og ikke om du har tilgang til den. For å sjekke tilgang, kan du bruke funksjonen `fs/can-access?`.

## Se også
- [Clojure Docs - fs/exists?](https://clojuredocs.org/clojure.java.io/exists_q)
- [Clojure Docs - io/resource](https://clojuredocs.org/clojure.java.io/resource)
- [Clojure Docs - fs/can-access?](https://clojuredocs.org/clojure.java.io/can-access_q)