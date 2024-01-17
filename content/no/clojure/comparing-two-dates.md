---
title:                "Sammenligning av to datoer"
html_title:           "Clojure: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligning av to datoer er en måte for programmerere å sammenligne to tidspunkter eller datoer og bestemme deres forhold. Dette kan være nyttig for å håndtere tidsdata, utløpsdatoer eller for å avgjøre rekkefølgen av hendelser.

## Hvordan:
La oss si vi vil sammenligne to datoer, 1. januar 2020 og 1. januar 2021, for å se om de er like eller ikke. Vi kan bruke Clojure-funksjonen "clojure.core/same?" for å sammenligne datoene og få et boolsk resultat tilbake.
```Clojure
(clojure.core/same? (java.time.Local