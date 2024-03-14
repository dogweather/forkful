---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:08.871948-07:00
description: "YAML, et rekursivt akronym for \"YAML Ain't Markup Language,\" er et\
  \ menneskelesbart data-serialiseringsformat brukt for konfigurasjonsfiler og\u2026"
lastmod: '2024-03-13T22:44:40.422051-06:00'
model: gpt-4-0125-preview
summary: "YAML, et rekursivt akronym for \"YAML Ain't Markup Language,\" er et menneskelesbart\
  \ data-serialiseringsformat brukt for konfigurasjonsfiler og\u2026"
title: Arbeider med YAML
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML, et rekursivt akronym for "YAML Ain't Markup Language," er et menneskelesbart data-serialiseringsformat brukt for konfigurasjonsfiler og datautveksling mellom språk med forskjellige datastrukturer. Programmerere benytter seg av YAML på grunn av dets enkelhet og lesbarhet, noe som gjør det til et ideelt valg for å konfigurere applikasjoner og legge til rette for datautveksling i flerspråklige programmeringsmiljøer.

## Hvordan:

Clojure inkluderer ikke innebygd støtte for YAML, men du kan bruke tredjepartsbiblioteker som `clj-yaml` for parsing og generering av YAML-data. Først, legg til biblioteket i prosjektavhengighetene dine:

```clojure
;; Legg dette til i dine project.clj avhengigheter
[clj-yaml "0.7.0"]
```

Her er hvordan du kan bruke `clj-yaml` til å parse YAML og konvertere Clojure maps til YAML.

### Parse YAML:

```clojure
(require '[clj-yaml.core :as yaml])

;; Parse en YAML-streng
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Utdata:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Generere YAML fra Clojure:

```clojure
(require '[clj-yaml.core :as yaml])

;; Konvertere et Clojure map til en YAML-streng
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Utdata:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Disse enkle operasjonene med `clj-yaml` kan integreres i Clojure-applikasjoner for å håndtere konfigurasjonsfiler eller legge til rette for datautveksling med andre tjenester eller komponenter som bruker YAML.
