---
title:                "Arbeta med yaml"
html_title:           "Clojure: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML står för "YAML Ain't Markup Language" och är ett sätt att strukturera och representera data i ett läsbart format för både människor och datorer. Det används ofta i programmering för att konfigurera och kommunicera mellan olika program och system.

## Hur man gör:
```Clojure
(require '[clojure.data.yaml :as yaml])

(def data {:name "John" :age 30 :hobbies ["coding" "gaming" "reading"]})

(yaml/generate-string data)
;; => "name: John\nage: 30\nhobbies:\n- coding\n- gaming\n- reading\n"
```

YAML kan också användas för att läsa in data från en textfil:

```Clojure
(def text (slurp "data.yaml"))

(yaml/parse-string text)
;; => {:name "John" :age 30 :hobbies ["coding" "gaming" "reading"]}
```

YAML är också användbart för att skapa datastrukturer som är läsbara av flera programmeringsspråk, till exempel kan en Python-utvecklare läsa samma YAML-fil och tolka den på samma sätt. 

## Djupdykning:
YAML skapades i början av 2000-talet som en ersättning för de mer komplexa XML- och JSON-formaten. Det är lättare att läsa och skriva för människor eftersom det inte använder sig av speciella taggar och hakparenteser. 

Som en alternativ metod för att strukturera data, används också EDN (Extensible Data Notation) inom Clojure. Det är enbart skrivbart i Clojure och inte lika populär som YAML.

YAML bygger på en parser som är implementerad i Clojure. Det finns också andra YAML-bibliotek tillgängliga för Clojure, men det inbyggda biblioteket är den vanligaste metoden att använda YAML.

## Se även:
- YAML-specifikationen: https://yaml.org/spec/
- YAML-mode för Emacs: https://github.com/yoshiki/yaml-mode
- Yaml-clojure på GitHub: https://github.com/technomancy/yaml-clojure