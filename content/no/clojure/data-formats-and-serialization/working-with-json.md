---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:29.229236-07:00
description: "Hvordan: Clojure inkluderer ikke innebygde funksjoner for \xE5 jobbe\
  \ med JSON, s\xE5 du vil typisk bruke tredjepartsbiblioteker. `cheshire` og `jsonista`\
  \ er\u2026"
lastmod: '2024-03-13T22:44:40.423097-06:00'
model: gpt-4-0125-preview
summary: "Clojure inkluderer ikke innebygde funksjoner for \xE5 jobbe med JSON, s\xE5\
  \ du vil typisk bruke tredjepartsbiblioteker."
title: Arbeider med JSON
weight: 38
---

## Hvordan:
Clojure inkluderer ikke innebygde funksjoner for å jobbe med JSON, så du vil typisk bruke tredjepartsbiblioteker. `cheshire` og `jsonista` er populære valg på grunn av deres brukervennlighet og ytelse.

### Bruke Cheshire
Først, legg Cheshire til dine prosjektavhengigheter i `project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

For å analysere en JSON-streng til et Clojure-map og konvertere et map til en JSON-streng:

```clj
(require '[cheshire.core :as json])

;; Parse JSON-streng til Clojure-map
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Konverter Clojure-map til JSON-streng
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Bruke Jsonista
Legg Jsonista til ditt prosjekt `project.clj`:
```clj
[jsonista "0.3.2"]
```

Lignende operasjoner med Jsonista:

```clj
(require '[jsonista.core :as j])

;; Parse JSON-streng til Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Konverter Clojure-map til JSON-streng
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

I begge bibliotekene har du muligheten til å kode og dekode mer komplekse datastrukturer, og det er flere funksjoner og parametere som tillater tilpasning av serialisering- og deserialiseringsprosessene. For de fleste applikasjoner gir den demonstrerte funksjonaliteten et solid grunnlag for å jobbe med JSON i Clojure-applikasjoner.
