---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Clojure: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på om en mappe eksisterer i Clojure-kode? Det kan være nyttig å sjekke dette i tilfelle feil, eller hvis du trenger å gjøre noe spesifikt med innholdet i mappen.

## Hvordan gjøre det

```Clojure
;; Først må du importere biblioteket for å jobbe med filer og mapper
(ns user
  (:require [clojure.java.io :as io]))

;; Sjekk om mappen eksisterer ved å bruke funksjonen "file?" og gi banen til mappen som argument
(file? "sti/til/mappen")

;; Hvis mappen eksisterer, vil konsollen returnere "true", hvis ikke, vil det returnere "false"
```

For eksempel, hvis du vil sjekke om mappen "bilder" eksisterer i "bilder/mappe1/mappe2" i ditt prosjekt, kan du bruke følgende kode:

```Clojure
(file? "bilder/mappe1/mappe2/bilder")
```

Den vil returnere "true" hvis mappen eksisterer, og "false" hvis ikke.

## Deep Dive

Under overflaten, bruker "file?"-funksjonen Clojure sin "clojure.java.io" bibliotek, spesifikt funksjonen "file-seq". Dette konverterer banen til en fil og bruker deretter "ex-"prefiks på funksjonen som sjekker om filen eksisterer. Hvis det eksisterer, returneres filen, hvis ikke, returneres "nil".

## Se også

- Clojure sin dokumentasjon for Java sin "io"-bibliotek: https://clojure.github.io/clojure/clojure.java.io-api.html
- En artikkel om hvordan man kan jobbe med filer og mapper i Clojure: https://www.habrador.com/tutorials/clojure-file-io/