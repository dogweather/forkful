---
title:                "Å skrive en tekstfil"
html_title:           "Clojure: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en vanlig oppgave for programmerere, enten det er for å lagre data, generere rapporter eller lage dokumentasjon. I Clojure, et populært funksjonelt programmeringsspråk, er det flere måter å skrive tekstfiler på, avhengig av bruksområdet ditt.

## Slik gjør du det

For å skrive en tekstfil i Clojure, kan du bruke funksjonen `spyt` (også kjent som `slurp-back`) fra `clojure.java.io`-biblioteket. Denne funksjonen tar inn en streng med filnavnet som argument og returnerer en streng med innholdet i filen.

La oss si at vi har en tekstfil med navnet "minfil.txt" og innholdet "Hei, dette er en tekstfil." For å lese innholdet kan vi bruke følgende kode:

```Clojure
(ns minprosjekt.core
(:require [clojure.java.io :as io]))

(defn les-fil []
  (let [filnavn "minfil.txt"
        innhold (io/spyt filnavn)]
    println innhold))

(les-fil)
```

Når vi kjører denne koden, vil det bli skrevet ut "Hei, dette er en tekstfil." i terminalen. Merk at `spyt` fungerer bare for tekstfiler, ikke for binære filer.

Hvis du vil legge til tekst i en eksisterende fil, kan du bruke funksjonen `spyta` fra samme bibliotek. Denne funksjonen tar inn en streng med filnavnet og en annen streng med teksten du vil legge til som argumenter.

La oss si at vi vil legge til "Dette er en ny linje i tekstfilen." i "minfil.txt". Koden vil se slik ut:

```Clojure
(ns minprosjekt.core
(:require [clojure.java.io :as io]))

(defn legg-til-i-fil []
  (let [filnavn "minfil.txt"
        tekst "Dette er en ny linje i tekstfilen."
        (io/spyta filnavn tekst)]
    (println "Teksten er lagt til i filen.")))

(legg-til-i-fil)
```

Når vi kjører denne koden, vil "Dette er en ny linje i tekstfilen." bli lagt til i slutten av "minfil.txt".

## Gå dypere

I tillegg til `spyt` og `spyta`, finnes det andre metoder for å skrive tekstfiler i Clojure. For mer komplekse filer kan du bruke `clojure.data.json`-biblioteket for å skrive og lese JSON-filer, og `clojure.edn`-biblioteket for å skrive og lese EDN-filer.

Det finnes også flere biblioteker for å lage og manipulere CSV-filer, som `clojure/data.csv` og `incanter-csv`.

Ved å bruke disse verktøyene kan du enkelt lage og manipulere tekstfiler i Clojure, og utføre ulike oppgaver som krever tekstbehandling.

## Se også

- [Clojure official website](https://clojure.org/)
- [Clojure API reference](https://clojure.github.io/clojure/)
- [Java interop in Clojure](https://clojure.org/reference/java_interop)