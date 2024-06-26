---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:10.912667-07:00
description: "Hoe: In Clojure is het aanmaken en manipuleren van associatieve arrays\
  \ (hashmap's) eenvoudig. Laten we beginnen met wat voorbeelden. Om een hashmap te\u2026"
lastmod: '2024-03-13T22:44:50.411888-06:00'
model: gpt-4-0125-preview
summary: In Clojure is het aanmaken en manipuleren van associatieve arrays (hashmap's)
  eenvoudig.
title: Gebruik van associatieve arrays
weight: 15
---

## Hoe:
In Clojure is het aanmaken en manipuleren van associatieve arrays (hashmap's) eenvoudig. Laten we beginnen met wat voorbeelden.

Om een hashmap te maken:

```clojure
(def mijn-map {:naam "Alex" :leeftijd 30})
```

Je kunt een waarde ophalen door de sleutel op te geven:

```clojure
(get mijn-map :naam)
;; "Alex"
```
Of, idiomatischer, je kunt de sleutel als een functie gebruiken:

```clojure
(:naam mijn-map)
;; "Alex"
```

Het toevoegen of bijwerken van items is eenvoudig:

```clojure
(def bijgewerkte-map (assoc mijn-map :locatie "New York"))
;; {:naam "Alex", :leeftijd 30, :locatie "New York"}

(def verhoogde-leeftijd (update mijn-map :leeftijd inc))
;; {:naam "Alex", :leeftijd 31}
```

Voor het verwijderen van sleutels, gebruik `dissoc`:

```clojure
(def verwijderde-leeftijd (dissoc mijn-map :leeftijd))
;; {:naam "Alex"}
```

Om over een map te itereren:

```clojure
(doseq [[k v] mijn-map] (println k "->" v))
;; :naam -> Alex
;; :leeftijd -> 30
```

En voor voorwaardelijke toegang, `find` geeft een sleutel-waardepaar terug als de sleutel bestaat:

```clojure
(find mijn-map :leeftijd)
;; [:leeftijd 30]
```

## Diepgaande duik
Associatieve arrays in Clojure, ook vaak aangeduid als hashmap's, zijn ongelooflijk veelzijdig en efficiënt voor het beheer van data op basis van sleutel-waarden. Ze maken deel uit van de rijke collectiebibliotheek van Clojure, diep geworteld in de filosofie van de taal van onveranderlijkheid en functioneel programmeren. In tegenstelling tot arrays of lijsten die O(n) tijdscomplexiteit vereisen voor toegang tot elementen, bieden hashmap's bijna constante tijdscomplexiteit voor toegang, waardoor ze zeer efficiënt zijn voor opzoekoperaties.

Men zou kunnen betogen dat vectoren in Clojure een soortgelijk doel zouden kunnen dienen via geïndexeerde toegang, maar hashmap's blinken uit bij het omgaan met niet-opeenvolgende en gelabelde data, waar de sleutel een betekenisvolle descriptor biedt in plaats van een willekeurige index.

Uniek voor Clojure (en haar Lisp-erfgoed), associatieve arrays zijn first-class citizens, wat betekent dat ze rechtstreeks kunnen worden gemanipuleerd, rondgegeven in functies, en meer, zonder dat er speciale syntax of toegangsmethoden nodig zijn. Deze ontwerpbeslissing versterkt de nadruk van Clojure op eenvoud en kracht.

Hoewel hashmap's ongelooflijk nuttig zijn, is het vermeldenswaard dat voor zeer grote datasets of scenario's waarin sleutels zeer dynamisch zijn (constante toevoeging en verwijdering), alternatieve datastructuren of databases mogelijk betere prestaties en flexibiliteit bieden. Echter, voor de meeste typische gebruiksscenario's binnen het domein van Clojure-applicaties, bieden associatieve arrays een robuust en efficiënt middel voor datamanagement.
