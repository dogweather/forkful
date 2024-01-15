---
title:                "Jobbe med YAML"
html_title:           "Clojure: Jobbe med YAML"
simple_title:         "Jobbe med YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med datastrukturer og ønsker å ha en enkel og lesbar måte å lagre og dele data på, så er YAML et flott valg. Med YAML kan du enkelt organisere, strukturere og manipulere data på en intuitiv måte.

## Hvordan Du Gjør Det

```Clojure
(ns yaml-demo.core
  (:require [clojure-yaml.core :as yaml]))

;; Lesing av YAML-fil
(def data (yaml/read-yaml "data.yml"))

;; Skrive YAML-fil
(yaml/spit-yaml "resultat.yml" 
  {"navn" "Eirik", "alder" 25, "hobbyer" ["programmering" "musikk"]})

;; Konvertere Clojure datastrukturer til YAML
(yaml/generate-yaml
  {"navn" "Lise", "bosted" "Oslo", "hobby" "fotografi"})

;; Resultat:
;; navn: Lise
;; bosted: Oslo
;; hobby: fotografi 

```

## Dypdykk

YAML står for "YAML Ain't Markup Language" og ble utviklet på begynnelsen av 2000-tallet for å gi et enkelt og øyevennlig alternativ til XML og JSON. YAML har en tydelig og intuitiv syntaks som er inspirert av datamodelleringsspråket JSON.

YAML støtter også forskjellige datastrukturer som lister, maps og sets, og det er mulig å inkludere referanser og innebygde datastrukturer. Med YAML kan du også kommentere koden din, noe som gjør det enkelt å dokumentere datastrukturen din.

## Se Også

- [Offisiell YAML-hjemmeside](https://yaml.org/)
- [YAML-dokumentasjon i Clojure](https://github.com/clj-commons/clojure-yaml)
- [En sammenligning mellom YAML, JSON og XML](https://www.tutorialspoint.com/yaml/yaml_comparison.htm)