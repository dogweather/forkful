---
title:                "Clojure: Å jobbe med YAML"
simple_title:         "Å jobbe med YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML er et populært dataformat for konfigurasjonsfiler i Clojure-programmering. Ved å lære å arbeide med YAML, kan du enkelt lese og manipulere dataene dine på en strukturert og intuitiv måte.

## Hvordan å arbeide med YAML i Clojure

Det første trinnet for å jobbe med YAML i Clojure er å importere biblioteket `clj-yaml` ved å legge til følgende linje i `project.clj`-filen din:

```Clojure
[clj-yaml "0.6.0"]
```

Etter å ha konfigurert prosjektet ditt, kan du begynne å bruke `clj-yaml` for å lese og skrive YAML-filer. La oss si at du har en YAML-fil med følgende innhold:

```Clojure
{:name "Jane"
 :age 25
 :hobbies ["reading" "cooking" "hiking"]}
```

For å lese denne filen, kan du bruke `parse-string`-funksjonen:

```Clojure
(require '[clj-yaml.core :refer [parse-string]])

(def data (parse-string "Jane.yaml"))
```

Du kan nå få tilgang til dataene ved å bruke Clojure-funksjoner som `get` eller `get-in`:

```Clojure
(get data :name) ; "Jane"
(get-in data [:hobbies 1]) ; "cooking"
```

På samme måte kan du bruke `generate-string`-funksjonen for å generere YAML-data fra et Clojure-datastruktur:

```Clojure
(def person {:name "John"
             :age 30
             :hobbies ["coding" "gaming" "traveling"]})

(require '[clj-yaml.core :refer [generate-string]])

(def yaml (generate-string person)) ; "- coding\ngaming\n- traveling"
```

## Dykk dypere inn i YAML

For å gå enda dypere inn i YAML-programmering, kan du sjekke ut dokumentasjonen for `clj-yaml`-biblioteket og utforske flere funksjoner og muligheter. Du kan også lære mer om YAML-syntaksen og ulike måter å strukturere data på ved å lese ressurser på nettet.

See Also (Se Også):
- [Dokumentasjon for clj-yaml](https://github.com/yogthos/clj-yaml)
- [YAML-spesifikasjonen](https://yaml.org/spec/)
- [YAML-primer](https://learnxinyminutes.com/docs/yaml/)