---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Midlertidige Filer i Clojure: En Rask Guide

## Hva & Hvorfor?

Å opprette en midlertidig fil er en metode som benyttes av programmerere for lagring av data midlertidig under kjøretiden til applikasjonen. Det tjener typisk scenarioer der det er behov for kortvarig lagring uten å oppta varig minne.

## Hvordan:

Her er en grunnleggende måte å lage en midlertidig fil på i Clojure:

```clojure
(require '[clojure.java.io :as io])

(defn create-temp-file 
  []
  (let [temp (java.io.File/createTempFile "my-temp-file" ".txt")]
    (spit temp "This is a test!")
    (.deleteOnExit temp)
    temp))
```

Og du henter innholdet fra den midlertidige filen slik:

```clojure
(defn read-temp-file [file]
  (with-open [rdr (io/reader file)]
    (doseq [line (line-seq rdr)]
      (println line))))

(read-temp-file (create-temp-file))
```

Output:

```
This is a test!
```

## Dypere Dykk:

Oppretting av midlertidige filer har vært en nødvendig del av programmering siden tidlig dager på grunn av begrenset minnekapasitet. Selv om det i dag er rikelig med minne i de fleste systemer, er midlertidige filer fortsatt nyttige for sikker lagring og deling av data mellom prosesser.

Alternativt, kan du bruke biblioteker som `clojure.java.io` for å håndtere midlertidige filer på en enklere måte. Dette er mer hensiktsmessig for større applikasjoner.

Det er viktig å merke seg at programmereren er ansvarlig for sikker opprydding av disse filene. I Java og dermed Clojure, gjøres dette vanligvis ved hjelp av metoden `.deleteOnExit()`.

## Se Også:

1. [Offisiell clojure.java.io Dokumentasjon](https://clojure.github.io/clojure/clojure.java.io-api.html)
3. [Oracle Java Dokument – deleteOnExit()](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#deleteOnExit())