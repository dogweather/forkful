---
title:                "Clojure: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil kan være nyttig når du lagrer data som ikke trenger å være strukturert på en bestemt måte, eller når du ønsker å importere data fra en ekstern kilde inn i ditt Clojure-program. I denne blogginnlegget vil vi gå gjennom hvordan du kan lese en tekstfil ved hjelp av Clojure, og gi deg en dypere forståelse av hvordan dette kan gjøres.

## Hvordan

For å lese en tekstfil i Clojure, bruker vi funksjonen `slurp`, som tar inn filstien som et argument og returnerer tekstinnholdet i filen som en streng. La oss si at vi har en tekstfil som heter "minfil.txt" og inneholder følgende tekst:

```
Dette er en tekstfil
med litt innhold
og flere linjer
```

Vi kan lese denne filen i Clojure ved å bruke følgende kode:

```Clojure
(def tekst (slurp "minfil.txt"))
```

Dette vil lagre innholdet i filen som en streng i variabelen "tekst". Vi kan deretter skrive ut innholdet ved å bruke `println`-funksjonen:

```Clojure
(println tekst)
```

Dette vil gi følgende output:

```
Dette er en tekstfil
med litt innhold
og flere linjer
```

Vi kan også lese data fra en tekstfil på en mer strukturert måte, for eksempel ved å bruke funksjonen `clojure.string/split-lines`, som deler opp en streng etter hver linje:

```Clojure
(def linjer (clojure.string/split-lines tekst))
```

Dette vil gi oss en liste med alle linjene i filen, som vi kan bruke i vårt program.

## Dypdykk

Hvis du ønsker å lese en tekstfil som ikke er på ditt lokale filsystem, men i en ekstern kilde, kan du bruke "java.io" biblioteket i Clojure. Denne gir funksjoner for å lese og skrive til filer i forskjellige formater. For å lese en ekstern tekstfil kan vi bruke funksjonen `java.io/BufferedReader` sammen med `clojure.java.io/reader`:

```Clojure
(def reader (clojure.java.io/reader "https://www.mittnettsted.com/minfil.txt"))
(def tekst (java.io/BufferedReader. reader))
(println (slurp tekst)))
```

Dette vil lese innholdet i tekstfilen fra den eksterne kilden og skrive det ut på samme måte som vist tidligere.

## Se også

- [Clojure - Official Website](https://clojure.org/)
- [Clojure Dokumentasjon](https://clojure.org/documentation)
- [Clojure Cookbook - Lesing og Skriving av Filer](https://clojure-cookbook.com/reading-files.html)