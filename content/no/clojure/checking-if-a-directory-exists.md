---
title:                "Clojure: Sjekke om en mappe eksisterer"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger når man skriver et program, kan det være nyttig å sjekke om en spesifikk mappe eksisterer på datamaskinen din. Dette kan hjelpe med å sikre at programmet ditt kjører som forventet og kan bidra til å unngå feil og komplikasjoner.

## Hvordan

Det er flere måter du kan sjekke om en mappe eksisterer ved hjelp av Clojure. La oss se på et eksempel:

```Clojure
(ns my-program.core
  (:require [clojure.java.io :as io])) ; Importer nødvendige biblioteker

(def file-path "/bruker/navn/dokumenter/eksempel-mappe") ; Angi banen til mappen du vil sjekke

(if (.exists (io/file file-path)) ; Kjør en sjekk om mappen eksisterer
  (println "Mappen eksisterer.")  ; Hvis den gjør det, skriv ut en melding til konsollen
  (println "Mappen eksisterer ikke.")) ; Hvis den ikke gjør det, skriv ut en annen melding
```

La oss se nærmere på denne koden. Først importerer vi biblioteket `clojure.java.io` som gir oss funksjonalitet for å håndtere input og output. Deretter definerer vi `file-path` variabelen og angir banen til mappen vi vil sjekke. Dette kan endres til den faktiske banen på datamaskinen din.

Vi bruker deretter en `if`-setning for å sjekke om mappen eksisterer. Hvis den gjør det, vil en melding bli skrevet ut til konsollen. Hvis ikke, vil en annen melding bli skrevet ut.

Du kan også bruke Clojure's `clojure.java.io.file` funksjoner for å sjekke om en mappe eksisterer. For eksempel:

```Clojure
(require '[clojure.java.io :as io])

(when (io/file-exists? "/bruker/navn/dokumenter/eksempel-mappe")
  (println "Mappen eksisterer."))
```

Her bruker vi `when`-setningen som bare vil kjøre koden hvis mappen eksisterer. Ellers vil den hoppe over koden.

## Dypdykk

Når du sjekker om en mappe eksisterer, bruker Clojure egentlig Java's `java.io.File` klassen. Dette gir deg flere alternativer når du sjekker for mappen, som å sjekke om det er en fil og ikke en mappe, eller å få tilgang til metadata.

Det er også flere biblioteker som du kan bruke for å håndtere filer og mapper i Clojure, som `clojure.java.io`, `clojure.contrib` og `clj-fs`.

## Se også

- [clojure.java.io dokumentasjon](https://clojuredocs.org/clojure.java.io/file)
- [clojure.contrib dokumentasjon](https://clojuredocs.org/clojure.contrib)
- [clj-fs dokumentasjon](https://github.com/wkf/clj-fs)