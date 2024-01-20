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

Å sjekke om en mappe eksisterer betyr ganske enkelt å bekrefte at en bestemt katalog finnes på en angitt sti i et filsystem. Dette gjør programmerere for å unngå feil ved forsøk på å lese fra eller skrive til en ikke-eksisterende katalog.

## Slik gjør du:

I Clojure kan du bruke `java.nio.file` biblioteket for å sjekke om en mappe eksisterer. Her er et eksempel:

```Clojure
(import 'java.nio.file.Files 'java.nio.file.Paths)

(defn directory-exists? [dir]
  (Files/exists (Paths/get dir (into-array String []))))
```
Hvis katalogen eksisterer, vil funksjonen `directory-exists?` returnere `true`:
```Clojure
(directory-exists? "/min/eksempel/mappe") ;; blir enten true eller false
```
## Dypdykk

Clojure, som en variant av Lisp- og på JVM, gir direkte tilgang til Java's plattformfunksjoner, inkludert `java.nio.file` biblioteket som brukes her. Selv om det er andre måter å sjekke om en mappe eksisterer i Java, som `java.io.File`, tilbyr `java.nio.file` mer effektive og robuste alternativer for fil- og mappeoperasjoner.

Implementeringen over bruker `Files/exists` metoden fra `java.nio.file.Files` klasse, og `Paths/get` metoden fra `java.nio.file.Paths` klasse. Vi konverterer stien til en `java.nio.file.Path` instans, og bruker deretter `Files/exists` metoden for å sjekke om katalogen eksisterer.

## Se også

For mer informasjon om fil og mappe operasjoner i Clojure og Java, se:

- [java.nio.file.Files dokumentasjon](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
- [java.nio.file.Paths dokumentasjon](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Paths.html)
- [Clojure's java interop dokumentasjon](https://clojure.org/reference/java_interop) 

Vær oppmerksom på at du kan ulike variasjoner av ovenstående funksjon basert på dine spesifikke behov, inkludert å kontrollere spesifikke filtyper, sjekke lese- eller skrivetilgang, og så videre.