---
title:                "Arbeta med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON, som är dataformatet alla pratar om, gör det enkelt att spara och utbyta data. Programmerare använder det för att jobba med data på webben, mellan olika språk och plattformar.

## Hur gör man:
I Clojure hanterar vi JSON med bibliotek som `cheshire`. Installera det genom att lägga till `[cheshire "5.10.1"]` i ditt `project.clj`. Så här ser kod och output ut:

```Clojure
(require '[cheshire.core :as json])

;; Konvertera Clojure map till JSON sträng
(json/encode {:a 1 :b true :c "clojure"})
;; Output: "{\"a\":1,\"b\":true,\"c\":\"clojure\"}"

;; Parse JSON sträng till Clojure map
(json/decode "{\"a\":1,\"b\":true,\"c\":\"clojure\"}" true)
;; Output: {:a 1, :b true, :c "clojure"}
```

## Djupdykning
JSON utvecklades tidigt 2000-tal som ett enklare alternativ till XML. I Clojureland finns andra alternativ som `data.json` och `jsonista`. Cheshire använder Jackson-biblioteket under huven för snabbhet och extra funktioner, som custom encoders.

## Se Också
* Cheshire GitHub Repo: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
* Clojure's `data.json`: [https://github.com/clojure/data.json](https://github.com/clojure/data.json)
* Jsonista: [https://github.com/metosin/jsonista](https://github.com/metosin/jsonista)
* Jackson-dokumentation: [https://github.com/FasterXML/jackson](https://github.com/FasterXML/jackson)
