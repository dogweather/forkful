---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:49.449509-07:00
description: "Hur man g\xF6r: I Clojure \xE4r det enkelt att skapa och manipulera\
  \ associativa arrayer (hashkartor). L\xE5t oss dyka in med exempel. F\xF6r att skapa\
  \ en hashkarta."
lastmod: '2024-03-13T22:44:37.518469-06:00'
model: gpt-4-0125-preview
summary: "I Clojure \xE4r det enkelt att skapa och manipulera associativa arrayer\
  \ (hashkartor)."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
I Clojure är det enkelt att skapa och manipulera associativa arrayer (hashkartor). Låt oss dyka in med exempel.

För att skapa en hashkarta:

```clojure
(def my-map {:name "Alex" :age 30})
```

Du kan hämta ett värde genom att specificera dess nyckel:

```clojure
(get my-map :name)
;; "Alex"
```
Eller, mer idiomatiskt, kan du använda nyckeln som en funktion:

```clojure
(:name my-map)
;; "Alex"
```

Att lägga till eller uppdatera poster är enkelt:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

För att ta bort nycklar, använd `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

För att iterera över en karta:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

Och för villkorlig tillgång ger `find` ett nyckel-värde-par om nyckeln finns:

```clojure
(find my-map :age)
;; [:age 30]
```

## Fördjupning
Associativa arrayer i Clojure, även ofta benämnda som hashkartor, är otroligt mångsidiga och effektiva för att hantera nyckel-värde-baserad data. De är en del av Clojures rika samling av bibliotek, djupt rotade i språkets filosofi kring oföränderlighet och funktionell programmering. Till skillnad från arrayer eller listor som kräver O(n) tidskomplexitet för tillgång till element, erbjuder hashkartor nästan konstant tidskomplexitet för tillgång, vilket gör dem högeffektiva för uppslagsoperationer.

Man kan argumentera för att vektorer i Clojure kan tjäna ett liknande syfte genom indexerad tillgång, men hashkartor lyser när det kommer till att hantera icke-sekventiell och märkt data, där nyckeln ger en meningsfull beskrivning snarare än ett godtyckligt index.

Unikt för Clojure (och dess Lisp-arv) är att associativa arrayer är förstaklassiga medborgare, vilket innebär att de kan manipuleras direkt, överföras mellan funktioner, och mer, utan att behöva särskild syntax eller tillgångsmetoder. Detta designbeslut förstärker Clojures betoning på enkelhet och kraft.

Även om hashkartor är otroligt användbara, är det värt att nämna att för mycket stora datasets eller scenarier där nycklar är mycket dynamiska (konstant tillägg och borttagning), kan alternativa datastrukturer eller databaser erbjuda bättre prestanda och flexibilitet. Dock, för de flesta typiska användningsfall inom ramen för Clojure-applikationer, tillhandahåller associativa arrayer ett robust och effektivt sätt att hantera data.
