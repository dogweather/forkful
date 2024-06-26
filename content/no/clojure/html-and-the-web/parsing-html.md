---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:14.751264-07:00
description: "Hvordan: Clojure har ikke innebygd evne til \xE5 tolke HTML, men du\
  \ kan utnytte Java-biblioteker eller Clojure-wrappere som `enlive` eller `hickory`.\
  \ Her er\u2026"
lastmod: '2024-03-13T22:44:40.400311-06:00'
model: gpt-4-0125-preview
summary: "Clojure har ikke innebygd evne til \xE5 tolke HTML, men du kan utnytte Java-biblioteker\
  \ eller Clojure-wrappere som `enlive` eller `hickory`."
title: Analysering av HTML
weight: 43
---

## Hvordan:
Clojure har ikke innebygd evne til å tolke HTML, men du kan utnytte Java-biblioteker eller Clojure-wrappere som `enlive` eller `hickory`. Her er hvordan du bruker begge:

### Bruke Enlive:
Enlive er et populært valg for HTML-tolkning og web-skrapping. Først, inkluder det i prosjektavhengighetene dine:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Deretter kan du tolke og navigere i HTML slik:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Dette kodeutsnittet henter en HTML-side og velger alle `<div>`-elementer med klassen `some-class`.

Utdata kan se slik ut:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Her er noe innhold."]})
```

### Bruke Hickory:
Hickory tilbyr en måte å tolke HTML på til et format som er lettere å jobbe med i Clojure. Legg Hickory til prosjektavhengighetene dine:

```clojure
[hickory "0.7.1"]
```

Her er et enkelt eksempel:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Tolke HTML-en til Hickory-format
(let [doc (hickory/parse "<html><body><div id='main'>Hei, verden!</div></body></html>")]
  ;; Velg div-en med id 'main'
  (select/select (select/id "main") doc))
```

Denne koden tolker en enkel HTML-streng og bruker en CSS-selektor for å finne en `div` med ID-en `main`.

Eksempelutdata:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hei, verden!"]}]
```

Både `enlive` og `hickory` tilbyr robuste løsninger for HTML-tolkning i Clojure, med `enlive` som fokuserer mer på templating og `hickory` som legger vekt på datatransformasjon.
