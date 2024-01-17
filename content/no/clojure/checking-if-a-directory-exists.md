---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Clojure: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekking av om en mappe eksisterer er en vanlig oppgave for programmere. Dette gjøres for å sikre at filoperasjoner kan gjennomføres uten feil og for å sørge for at riktig handling blir utført hvis mappen ikke eksisterer.

## Slik Gjør Du:
```Clojure
(def directory "/Users/username/documents")

(if (clojure.java.io/file directory)
  "Mappen finnes"
  "Mappen finnes ikke")
```
Output: "Mappen finnes"

## Dypdykk:
Det å sjekke om en mappe eksisterer har vært en del av programmering siden de første operative systemene ble utviklet. Det finnes også alternativer til å bruke Clojure sin innebygde funksjon for å sjekke om en mappe eksisterer, som for eksempel å bruke shell-kommandoer eller å bruke et bibliotek som spesialiserer seg på filbehandling. Implementasjonen av Clojure sin `file`funksjon vil variere fra system til system, da det er avhengig av hvordan systemet håndterer filbehandling.

## Se Også:
[Offisiell Dokumentasjon for clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)

[Clojure Cookbook for Filbehandling](https://clojure-cookbook.github.io/file-handling/)

[Funksjon for Sjekking av Filstatus i Clojure](https://github.com/pomin5/clj-fstat)