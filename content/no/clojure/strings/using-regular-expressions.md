---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:36.412245-07:00
description: "Hvordan: Clojure, som holder seg tro mot sine r\xF8tter i Lisp-familien,\
  \ tilbyr et rikt sett med funksjoner som fungerer s\xF8ml\xF8st med Java sine regul\xE6\
  re\u2026"
lastmod: '2024-03-13T22:44:40.392723-06:00'
model: gpt-4-0125-preview
summary: "Clojure, som holder seg tro mot sine r\xF8tter i Lisp-familien, tilbyr et\
  \ rikt sett med funksjoner som fungerer s\xF8ml\xF8st med Java sine regul\xE6re\
  \ uttrykkskapasiteter."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:
Clojure, som holder seg tro mot sine røtter i Lisp-familien, tilbyr et rikt sett med funksjoner som fungerer sømløst med Java sine regulære uttrykkskapasiteter. Her er hvordan du kan utnytte dem:

### Grunnleggende Sammenligning
For å sjekke om en streng matcher et mønster, bruk `re-matches`. Den returnerer hele treffet hvis vellykket eller `nil` ellers.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Søke etter Mønstre
For å finne den første forekomsten av et mønster, er `re-find` din go-to funksjon:

```clojure
(re-find #"\d+" "Ordre 123")  ;=> "123"
```

### Fangende Grupper
Bruk `re-find` sammen med parenteser i mønsteret ditt for å fange grupper:

```clojure
(let [[_ område kode] (re-find #"(1)?(\d{3})" "Telefon: 123-4567")]
  (println "Områdekode:" område "Kode:" kode))
;; Utdata: Områdekode: nil Kode: 123
```

### Global Søk (Finn Alle Treff)
Clojure har ikke en innebygd global søkefunksjon som noen språk. Bruk isteden `re-seq` for å få en lat sekvens av alle treff:

```clojure
(re-seq #"\d+" "id: 123, antall: 456")  ;=> ("123" "456")
```

### Splitting av Strenger
For å splitte en streng basert på et mønster, bruk `clojure.string/split`:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### Erstatning
Erstatt deler av en streng som matcher et mønster med `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "ÅÅÅÅ")  ;=> "ÅÅÅÅ-04-01"
```

### Biblioteker fra tredjeparter
Selv om Clojure sin innebygde støtte er tilstrekkelig for de fleste tilfeller, for mer komplekse scenarioer, vurder å bruke biblioteker som `clojure.spec` for robust datavalidering og `reagent` for reaktiv DOM-manipulering i webapplikasjoner med regex-basert ruting og inndata-validering.

```clojure
;; Eksempel som bruker clojure.spec for å validere en e-post
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> sant
```

Husk, mens regulære uttrykk er kraftfulle, kan de også gjøre kode vanskelig å lese og vedlikeholde. Bruk dem med omhu og vurder alltid enklere strengmanipuleringsfunksjoner hvor mulig.
