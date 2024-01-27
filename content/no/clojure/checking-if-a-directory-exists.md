---
title:                "Sjekke om en mappe finnes"
date:                  2024-01-19
html_title:           "Arduino: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Sjekking om en katalog eksisterer hjelper oss å unngå feil når filsystem operasjoner utføres. Det er viktig for kondisjonelle flyter og datapersistering.

## How to:
Clojure gjør det enkelt med `java.io.File` klassen:

```clojure
(import '[java.io File])

(defn directory-exists? [path]
  (.isDirectory (File. path)))

(println (directory-exists? "/et/eksisterende/sted")) ; => true
(println (directory-exists? "/et/ikke-eksisterende/sted")) ; => false
```
Output vil være `true` eller `false` avhengig av om katalogen finnes.

## Deep Dive
Fra starten, har Clojure levert effektive metoder for å samhandle med JVM og Java klasser. Sjekking av om kataloger eksisterer er ingen unntak. Denne funksjonaliteten er ikke spesifikk for Clojure – det låner fra Java IO-rammeverket. Alternative tilnærminger inkluderer bruk av `file-seq` for å generere en sekvens av filer i en katalog og sjekke om sekvensen er tom.

Når det gjelder underliggende implementeringsdetaljer, `File` klassen sjekker filsystemets metadata for å bestemme om en bestemt sti er en katalog. Ytelsen for denne operasjonen kan variere avhengig av filsystemets størrelse og type.

## See Also
- Clojure Docs om IO: https://clojuredocs.org/clojure.java.io
- Java `File` klasse: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Clojure `file-seq` funksjonen: https://clojuredocs.org/clojure.core/file-seq
