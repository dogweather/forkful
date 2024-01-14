---
title:                "Clojure: Opprettelse av en midlertidig fil"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skape midlertidige filer er en vanlig oppgave i mange programmeringsspråk, inkludert Clojure. Disse filene brukes midlertidig for å lagre data eller informasjon som kun trengs i en kort periode. Det kan være nyttig for å organisere og behandle data i et program, og sørger for at filene blir slettet etter bruk for å unngå rot og hindre konflikter i filnavn.

## Hvordan

For å opprette en midlertidig fil i Clojure, kan du bruke funksjonen `with-open` sammen med `clojure.java.io` biblioteket. Dette gjør at filen automatisk blir slettet når den blir lukket.

```Clojure
(require '[clojure.java.io :as io])

(with-open [temp-file (io/file "temp/file.txt")]
  (io/copy (io/resource "input.txt") temp-file)
  (println "Den midlertidige filen ble opprettet!")
  (io/delete-file temp-file))
```

I dette eksempelet vil en midlertidig fil med navnet "file.txt" bli opprettet i mappen "temp". Deretter kopieres innholdet fra filen "input.txt" (som må ligge i samme mappe som Clojure-filen) inn i den midlertidige filen. Når programmet er ferdig, vil den midlertidige filen bli slettet automatisk.

## Dykk dypere ned

Det er også mulig å bruke Java-klassen `java.io.File` direkte for å opprette en midlertidig fil.

```Clojure
(let [temp-file (java.io.File/createTempFile "prefix" ".txt")]
  (println (.getPath temp-file))
  (.delete temp-file))
```

Her brukes `createTempFile`-funksjonen til å lage en midlertidig fil med et prefiks og en filtype. Deretter blir stien til den midlertidige filen skrevet ut, og filen blir slettet når programmet er ferdig.

## Se også

- [Java Docs: java.io.File#createTempFile](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)
- [Clojure Docs: clojure.java.io/file](https://clojuredocs.org/clojure.java.io/file)
- [Tutorial: Creating and Deleting Files in Clojure](https://www.baeldung.com/clojure-create-and-delete-file)