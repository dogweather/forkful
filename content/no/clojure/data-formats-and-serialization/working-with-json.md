---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:29.229236-07:00
description: "\xC5 jobbe med JSON (JavaScript Object Notation) i Clojure inneb\xE6\
  rer \xE5 analysere JSON-strenger til Clojure-datastrukturer (maps, vektorer) og\
  \ omvendt. Denne\u2026"
lastmod: 2024-02-19 22:04:59.709267
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON (JavaScript Object Notation) i Clojure inneb\xE6rer\
  \ \xE5 analysere JSON-strenger til Clojure-datastrukturer (maps, vektorer) og omvendt.\
  \ Denne\u2026"
title: Arbeider med JSON
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med JSON (JavaScript Object Notation) i Clojure innebærer å analysere JSON-strenger til Clojure-datastrukturer (maps, vektorer) og omvendt. Denne oppgaven er grunnleggende for webtjenester, APIer og applikasjoner som trenger å kommunisere data i et strukturert, tekstbasert format fordi JSON er universelt anerkjent og støttet på tvers av forskjellige programmeringsmiljøer.

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
