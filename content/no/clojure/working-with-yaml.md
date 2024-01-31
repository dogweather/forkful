---
title:                "Arbeid med YAML"
date:                  2024-01-19
simple_title:         "Arbeid med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et dataformat som brukes til konfigurasjon og datautveksling. Programmerere bruker det fordi det er lett å lese og skrive, og fungerer godt med alle programmeringsspråk.

## Hvordan:
For å jobbe med YAML i Clojure, bruk for eksempel `clj-yaml` biblioteket.

```Clojure
(require '[clj-yaml.core :as yaml])

; Leser YAML fra en streng
(def yaml-streng "
- a
- b
- c")

(def liste (yaml/parse-string yaml-streng))
(println liste)
```

Output vil bli:
```
[a b c]
```

For å skrive til YAML, gjør du om en Clojure datastruktur:

```Clojure
(def clojure-data {:navn "Ola" :yrke "Utvikler" :ferdigheter ["Clojure" "YAML" "Javascript"]})

(def yaml-string (yaml/generate-string clojure-data))
(println yaml-string)
```

Output vil se slik ut:
```
navn: Ola
yrke: Utvikler
ferdigheter:
- Clojure
- YAML
- Javascript
```

## Dypdykk
YAML, "YAML Ain't Markup Language", ble skapt i 2001 som et enklere alternativ til XML. Andre alternativer inkluderer JSON og TOML. Ved implementering, er YAML ofte brukt for konfigurasjonsfiler grunnet sin klare visuelle struktur. Det er kritisk å huske at YAML kan tolke data på forskjellige måter avhengig av parseren, som kan skape inkompatibilitet mellom systemer.

## Se Også:
- YAMLs offisielle nettside: [https://yaml.org/](https://yaml.org/)
- `clj-yaml` GitHub-repositorium: [https://github.com/circleci/clj-yaml](https://github.com/circleci/clj-yaml)
- Clojure offisiell dokumentasjon: [https://clojure.org/](https://clojure.org/)
- En sammenlikning av dataserielisering formater: [https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats](https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats)
