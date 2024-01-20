---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å konvertere en dato til en streng betyr å forvandle en datoobjekt til en formatert tekstlinje. Programmerere gjør dette for å gjøre innhold mer forståelig for brukere, eller for å lagre data på en lesbar måte.

## Hvordan til:

Her er et kodeeksempel på en konvertering av dato til streng i Clojure.

```clojure
(use 'clj-time.format)
(defn date-to-string [date]
  (unparse (formatter "yyyy-MM-dd") date))
```

Dette eksemplet vil outputte:

```clojure
(date-to-string (org.joda.time.DateTime.))
;; => "2022-05-17"
```

## Dyp Dykk

1. Historisk Kontekst: Clojure bruker Joda Time, en forbedring for standard Java-dato og tid-klasser før Java 8. 
2. Alternativer: "java.time"-pakken i nyere Java-versjoner fungerer også i Clojure.
3. Implementasjonsdetaljer: "clj-time.format" er en Clojure-wrapper for Joda Time sine formatteringsmuligheter.

## Se Også

Ta en titt på disse lenkene for å lære mer:

- Clojure Dato og Tid: https://clojure.github.io/java-time/
- Joda-Time: https://www.joda.org/joda-time/ 
- Java.time i Clojure: https://github.com/dm3/clojure.java-time