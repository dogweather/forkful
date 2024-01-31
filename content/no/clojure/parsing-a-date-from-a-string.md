---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:35:21.864595-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Dato-parsing fra strenger er å gjøre lesbare datoer om til strukturerte data datamaskiner kan behandle. Vi gjør dette for å enkelt kunne manipulere og sammenligne datoer, lagre dem i databaser, og for å kommunisere mellom systemer.

## How to:
Clojure gir oss en rekke verktøy for å takle dato-parsing. La oss dykke rett inn i noen kodelinjer som viser hvordan dette kan gjøres.

```Clojure
(require '[clj-time.format :as fmt])
(require '[clj-time.coerce :as coerce])

; Definer et datoformat
(def custom-formatter (fmt/formatter "dd.MM.yyyy"))

; Parsing av en streng til en dato
(def parsed-date (fmt/parse custom-formatter "31.12.2022"))

; Viser datoen
(prn (str "Parsed date is: " parsed-date))

; Konverterer til java.util.Date for kompatibilitet
(def java-date (coerce/to-date parsed-date))

; Printer java.util.Date
(prn (str "Java Date: " java-date))
```

Output:
```
"Parsed date is: 2022-12-31T00:00:00.000Z"
"Java Date: Sat Dec 31 00:00:00 UTC 2022"
```

## Deep Dive
I eldre dager var java.util.Date veien å gå for dato-manipulasjon i JVM-språk, inkludert Clojure. Men java.util.Date hadde sine begrensninger og problemer. Joda-Time biblioteket ble introdusert for å fylle disse hullene. Clojure's clj-time bibliotek er en wrapper rundt Joda-Time og gir en mer funksjonell tilnærming som passer godt med Clojure's filosofi. 

Alternativer til clj-time inkluderer java.time biblioteket (kjent som JSR-310), som er inkludert i Java 8 og utover. Dette gir et omfattende API for dato og tid, og er ment å erstatte eldre verktøy som java.util.Date.

Når du parser en dato fra en streng, bør formatteren matche formatet på strengen nøyaktig. Det er også viktig å håndtere tidssoner riktig for å unngå feil.

## See Also
- `clj-time` GitHub-side: https://github.com/clj-time/clj-time
- ClojureDocs, en community-drevet dokumentasjon: https://clojuredocs.org/
- Java 8 Date and Time guide: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
