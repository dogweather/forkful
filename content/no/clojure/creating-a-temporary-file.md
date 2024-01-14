---
title:                "Clojure: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å lage midlertidige filer er en vanlig oppgave i programmering, spesielt når du jobber med store datasett eller trenger å lagre midlertidig informasjon under kjøring av et program.

## Hvordan lage midlertidige filer i Clojure
For å lage en midlertidig fil i Clojure, kan du bruke funksjonen `with-open` sammen med `io/resources` som lar deg opprette en ny fil i et midlertidig direktori på filsystemet. Her er et eksempel på hvordan du kan lage og skrive til en midlertidig fil:

```Clojure
(with-open [file (io/resource "mifil.txt")]
    (io/write file "Dette er innholdet i min midlertidige fil"))
```

Etter å ha kjørt dette eksemplet, vil filen `mifil.txt` bli opprettet i et midlertidig direktori, og teksten `Dette er innholdet i min midlertidige fil` vil bli skrevet til filen.

Det er viktig å merke seg at midlertidige filer som er opprettet ved hjelp av `io/resources` vil bli slettet automatisk når programmet ditt er ferdig med å kjøre.

## Et dypere dykk
I Clojure er det mulig å spesifisere en egen midlertidig mappe ved hjelp av funksjonen `with-temp-file`. Dette lar deg opprette midlertidige filer på et bestemt sted på filsystemet, i stedet for i systemets standard midlertidige mappe.

```Clojure
(with-temp-file "min/midlertidige/mappe" "midlertidigfil.txt" 
    (fn [fil]
        (io/write fil "Dette er innholdet i min midlertidige fil")))
```

I dette eksemplet opprettes filen `midlertidigfil.txt` i mappen `min/midlertidige/mappe` og teksten `Dette er innholdet i min midlertidige fil` blir skrevet til filen.

## Se også
- [Official Clojure documentation for temporary files](https://clojuredocs.org/clojure.java.io/file) (In English)
- [Blogginnlegg om midlertidige filer i Clojure](https://medium.com/write-a-blog/create-temporary-files-in-clojure-f51902d98caa)  (In English)