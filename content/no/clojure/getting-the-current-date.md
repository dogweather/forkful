---
title:                "Clojure: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få nåværende dato er en vanlig oppgave i mange programmeringsspråk, inkludert Clojure. Datoer er viktige i å håndtere tidspunkt og planlegge hendelser, og derfor er det nyttig å kunne få den nåværende datoen i et program.

## Hvordan

For å få nåværende dato i Clojure, kan du bruke funksjonen `java.util.Date`. Denne funksjonen gir et datobjekt som kan brukes til å få nåværende dato og klokkeslett.

```Clojure
(import 'java.util.Date)
(defn get-current-date []
  (let [today (Date.)]
    ; Få datoen som en streng
    (println (str "Dagens dato er " (.getDate today) "." (.getMonth today) "." (.getYear today)))
    ; Få klokkeslettet som en streng
    (println (str "Klokken er " (.getHours today) ":" (.getMinutes today) ":" (.getSeconds today)))))
(get-current-date)
```

Output:

```
Dagens dato er 25.11.2021
Klokken er 9:30:45
```

## Dypdykk

Det kan være nyttig å vite hvordan Clojure behandler datoer og klokkeslett under panseret. Clojure bruker Java sin `java.util.Date`-klasse for å håndtere datoer og klokkeslett, og denne klassen representerer et tidspunkt som millisekunder siden 1. januar 1970.

Det er også mulig å bruke funksjonen `now` fra biblioteket `clj-time` for å få nåværende dato og klokkeslett i Clojure. Denne funksjonen gir tilbake et datobjekt som er enklere å håndtere og har flere nyttige funksjoner for å jobbe med datoer og klokkeslett.

## Se også

- [Java dokumentasjon for java.util.Date](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- [Clojure dokumentasjon for datoer og klokkeslett](https://clojuredocs.org/clojure.core/date%20%26%20time)
- [clj-time biblioteket](https://github.com/clj-time/clj-time)