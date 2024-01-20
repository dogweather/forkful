---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML hanterar konfiguration och datautbyte, det används för sin läslighet och enkelhet. Programmerare gillar YAML för att smidigt ladda inställningar eller för att hantera data mellan olika språk och tjänster.

## Så här gör du:
Använd `clj-yaml`, ett Clojure-bibliotek för att läsa och skriva YAML.

```clojure
;; Lägg till beroendet i ditt projekt
(require '[clj-yaml.core :as yaml])

;; Läsa YAML-sträng
(let [yaml-str "greeting: Hej, Världen!"]
  (yaml/parse-string yaml-str))
;; => {"greeting" "Hej, Världen!"}

;; Skriva till YAML-sträng
(let [data {:farewell "Adjö och tack!"}]
  (yaml/generate-string data))
;; => "farewell: Adjö och tack!\n"
```

## Djupdykning
YAML startades runt 2001, med syfte att vara mer människo-läsbart än XML. Tom Preston-Werner, Githubs medgrundare, föreslog alternativet TOML, medan JSON är ett vanligare, lättviktigt alternativ för datautbyte. På Clojure-sidan används Java-bibliotek för att parsa och generera YAML via Java interop.

## Se också
- `clj-yaml` biblioteket på GitHub: https://github.com/clj-commons/clj-yaml
- YAML officiell sida: https://yaml.org
- Clojure's Java interoperabilitet guide: https://clojure.org/reference/java_interop