---
title:                "Clojure: Leser en tekstfil"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese og håndtere tekstfiler er en vanlig oppgave i mange programmeringsprosjekter. Det kan være nødvendig for å analysere data eller lese innholdet i en fil som en del av en større prosess. I denne bloggposten skal vi se på hvordan du enkelt kan lese tekstfiler med Clojure.

## Hvordan

For å lese en tekstfil i Clojure kan vi bruke funksjonen `slurp`. Denne funksjonen leser innholdet i en fil og returnerer det som en streng. La oss si at vi har en fil som heter "data.txt" med følgende innhold:

```
Dette er en enkel tekstfil.
Den har flere linjer med tekst.
```

Vi kan bruke `slurp`-funksjonen slik:

```Clojure
(def data (slurp "data.txt"))
```

Variabelen `data` vil nå inneholde strengen "Dette er en enkel tekstfil. Den har flere linjer med tekst.". Vi kan også spesifisere en filbane hvis filen ikke er i samme mappe som koden vår.

Vi kan også lese en fil linje for linje ved å bruke `with-open`-konstruksjonen. Dette er nyttig hvis vi har store filer som ikke kan leses inn i minnet på en gang. La oss si at vi har en fil med tall på hver linje og ønsker å regne ut summen av tallene. Vi kan gjøre det slik:

```Clojure
(def sum 0)
(with-open [reader (clojure.java.io/reader "numbers.txt")]
  (doseq [line (line-seq reader)]
    (let [num (Integer/parseInt line)]
      (def sum (+ sum num)))))
```

Variabelen `sum` vil nå inneholde summen av tallene i filen.

## Dypdykk

Når det kommer til å lese tekstfiler i Clojure, er det også verdt å nevne `clojure.java.io`-biblioteket. Dette biblioteket inneholder mange nyttige funksjoner for å håndtere IO-operasjoner, inkludert å lese og skrive til filer. Du kan også se på `java.nio.file`-biblioteket for å utføre mer avanserte operasjoner som filmanipulasjon og søking.

## Se også

- [Clojure Offisiell Dokumentasjon for slurp](https://clojuredocs.org/clojure.core/slurp)
- [Clojure Offisiell Dokumentasjon for with-open](https://clojuredocs.org/clojure.core/with-open)
- [Clojure Offisiell Dokumentasjon for java.io](https://clojuredocs.org/clojure.java.io)
- [Java NIO Tutorial](https://www.baeldung.com/java-nio-file-operations)