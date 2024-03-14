---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:57.403889-07:00
description: "YAML, ett rekursivt akronym f\xF6r \"YAML Ain't Markup Language\", \xE4\
  r ett m\xE4nniskol\xE4sbart data-serialiseringsformat som anv\xE4nds f\xF6r konfigurationsfiler\
  \ och\u2026"
lastmod: '2024-03-13T22:44:37.545459-06:00'
model: gpt-4-0125-preview
summary: "YAML, ett rekursivt akronym f\xF6r \"YAML Ain't Markup Language\", \xE4\
  r ett m\xE4nniskol\xE4sbart data-serialiseringsformat som anv\xE4nds f\xF6r konfigurationsfiler\
  \ och\u2026"
title: Att Arbeta med YAML
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, ett rekursivt akronym för "YAML Ain't Markup Language", är ett människoläsbart data-serialiseringsformat som används för konfigurationsfiler och datautbyte mellan språk med olika datastrukturer. Programmerare använder YAML på grund av dess enkelhet och läslighet, vilket gör det till ett idealiskt val för att konfigurera applikationer och underlätta datautbyte i polyglotta programmeringsmiljöer.

## Hur man:

Clojure inkluderar inte inbyggt stöd för YAML, men du kan använda tredjepartsbibliotek som `clj-yaml` för att tolka och generera YAML-data. Först, lägg till biblioteket i dina projektberoenden:

```clojure
;; Lägg detta till dina beroenden i project.clj
[clj-yaml "0.7.0"]
```

Så här kan du använda `clj-yaml` för att tolka YAML och konvertera Clojure-mapar till YAML.

### Tolka YAML:

```clojure
(require '[clj-yaml.core :as yaml])

;; Tolkar en YAML-sträng
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Utdata:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Generera YAML från Clojure:

```clojure
(require '[clj-yaml.core :as yaml])

;; Konverterar en Clojure-map till en YAML-sträng
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Utdata:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Dessa enkla operationer med `clj-yaml` kan integreras i Clojure-applikationer för att hantera konfigurationsfiler eller underlätta datautbyte med andra tjänster eller komponenter som använder YAML.
