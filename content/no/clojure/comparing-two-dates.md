---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligne to datoer betyr å vurdere om en er tidligere, senere eller samme som den andre. Programmet gjør dette for å kontrollere hendelsesrækkefølgen, beregne tidsrammen, og flere.

## Hvordan gjør du det:
La oss se på et enkelt eksempel med Java Interoperabilitet i Clojure.

```clojure 
(defn sammenlign-datoer [dato1 dato2] 
  (.compareTo dato1 dato2))
```
Her er en prøvekjøring:
```clojure 
(require '[clj-time.core :as t])
(def dato1 (t/date-time 2020 10 1)) 
(def dato2 (t/date-time 2021 10 1)) 
(sammenlign-datoer dato1 dato2)
```
Kode blokken vil skrive ut `-1`, som betyr at dato1 kommer før dato2.

## Dypdykk
Clojure, som opprinnelig ble utviklet i 2007, støtter Java Interoperabilitet, så man kan sammenligne to datoobjekter direkte. 

Alternativt, hvis du vil jobbe med ren Clojure-biblioteker, kan du bruke `clj-time` biblioteket. Der finnes det funksjoner som `before?`, `after?` og `equal?` 

Når man sammenligner to datoer, gjøres en enkel numerisk sammenligning mellom tidspunktene for de to datoene, som er millisekunder fra referansepunktet, 1. januar 1970 (Unix epoch).

## Se Også
Sjekk ut følgende lenker for mer informasjon:
- Clojure Dokumentasjon: https://clojure.org/
- clj-time Bibliotek: https://github.com/clj-time/clj-time
- Java Date Dokumentasjon: https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/util/Date.html