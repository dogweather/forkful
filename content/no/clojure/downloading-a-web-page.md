---
title:                "Å laste ned en nettside"
html_title:           "Clojure: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Nedlasting av en nettside er å hente informasjon fra en nettside ved hjelp av en datamaskin. Dette er nyttig for programmerere for å samle data eller automatisere oppgaver.

## Hvordan:
```Clojure
(require '[clojure.java.io :as io])
(io/copy (io/input-stream "https://www.example.com") (io/output-stream "example.html"))
```
Dette eksemplet viser hvordan du kan bruke Clojure til å laste ned en nettside og lagre den som en fil kalt "example.html". Du trenger bare å erstatte nettadressen med adressen for nettsiden du vil laste ned.

## Dypdykk:
Å laste ned nettsider fra internett har vært en viktig del av programmering i mange år. Det finnes også alternativer til Clojure for å gjøre dette, som for eksempel Python og Ruby. For å implementere dette i Clojure, bruker vi funksjonen "copy" fra Java biblioteket for å kopiere informasjonen fra nettsiden og lagre den som en fil.

## Se også:
- Offisiell "clojure.java.io" dokumentasjon: https://clojure.github.io/clojure/clojure.java.io-api.html
- En tutorial for å laste ned nettsider i Clojure: https://practicalli.github.io/clojure-webapps/about/01_2-html.html#downloading_an_existing_webpage