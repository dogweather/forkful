---
date: 2024-01-20 17:38:04.950570-07:00
description: "Hur man g\xF6r: Att konvertera str\xE4ngar till sm\xE5 bokst\xE4ver\
  \ \xE4r en standardoperation i de flesta programmeringsspr\xE5k, och Clojure \xE4\
  r inget undantag. Historiskt\u2026"
lastmod: '2024-04-05T21:53:38.840278-06:00'
model: gpt-4-1106-preview
summary: "Att konvertera str\xE4ngar till sm\xE5 bokst\xE4ver \xE4r en standardoperation\
  \ i de flesta programmeringsspr\xE5k, och Clojure \xE4r inget undantag."
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

## Hur man gör:
```Clojure
;; Använd `clojure.string/lower-case` för att konvertera till små bokstäver
(require '[clojure.string :as str])

;; Exempel på strängkonvertering
(def example-string "Hej Världen!")
(def lower-case-string (str/lower-case example-string))

;; Utskrift av det converterade strängen
(println lower-case-string) ;; => "hej världen!"
```

## Djupdykning
Att konvertera strängar till små bokstäver är en standardoperation i de flesta programmeringsspråk, och Clojure är inget undantag. Historiskt har behovet av att jämföra strängar utan att skiftläge påverkar resultatet varit viktigt, särskilt i databaser och sökmotorer.

Alternativt kan programmerare använda Java-metoder direkt tack vare Clojures interop-förmåga med Java:

```Clojure
(.toLowerCase "Hej Världen!") ;; => "hej världen!"
```

Detaljer kring implementering kan variera beroende på underliggande plattform och teckenuppsättning. Clojure använder JVM:ns metoder vilket innebär att den tar hänsyn till lokala inställningar och Unicode-standarder.

## Se även
- Clojure's official string API documentation: [clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)
- Oracle's Java String documentation for `.toLowerCase()`: [Java String Docs](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
