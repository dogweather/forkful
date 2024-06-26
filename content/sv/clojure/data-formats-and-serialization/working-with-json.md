---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:15.504959-07:00
description: "Hur g\xF6r man: Clojure inkluderar inte inbyggda funktioner f\xF6r att\
  \ arbeta med JSON, s\xE5 du kommer typiskt att anv\xE4nda tredjepartsbibliotek.\
  \ `cheshire` och\u2026"
lastmod: '2024-03-13T22:44:37.546510-06:00'
model: gpt-4-0125-preview
summary: "Clojure inkluderar inte inbyggda funktioner f\xF6r att arbeta med JSON,\
  \ s\xE5 du kommer typiskt att anv\xE4nda tredjepartsbibliotek."
title: Arbeta med JSON
weight: 38
---

## Hur gör man:
Clojure inkluderar inte inbyggda funktioner för att arbeta med JSON, så du kommer typiskt att använda tredjepartsbibliotek. `cheshire` och `jsonista` är populära val på grund av deras användarvänlighet och prestanda.

### Använda Cheshire
Först, lägg till Cheshire till dina projektoberoenden i `project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

För att tolka en JSON-sträng till en Clojure-karta och konvertera en karta till en JSON-sträng:

```clj
(require '[cheshire.core :as json])

;; Tolka JSON-sträng till Clojure-karta
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Konvertera Clojure-karta till JSON-sträng
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Använda Jsonista
Lägg till Jsonista i ditt projekt `project.clj`:
```clj
[jsonista "0.3.2"]
```

Liknande operationer med Jsonista:

```clj
(require '[jsonista.core :as j])

;; Tolka JSON-sträng till Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Konvertera Clojure-karta till JSON-sträng
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

I båda biblioteken har du möjligheten att koda och avkoda mer komplexa datastrukturer, och det finns ytterligare funktioner och parametrar som tillåter anpassning av serialiserings- och avserialiseringsprocesserna. För de flesta applikationer ger den demonstrerade funktionaliteten en solid grund för att arbeta med JSON i Clojure-applikationer.
