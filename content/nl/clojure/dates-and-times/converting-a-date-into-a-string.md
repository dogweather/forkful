---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:24.658761-07:00
description: "Een datum omzetten naar een string betekent het transformeren van een\
  \ datumobject naar leesbare tekst voor mensen. Programmeurs doen dit om data in\u2026"
lastmod: '2024-03-11T00:14:24.243010-06:00'
model: gpt-4-0125-preview
summary: "Een datum omzetten naar een string betekent het transformeren van een datumobject\
  \ naar leesbare tekst voor mensen. Programmeurs doen dit om data in\u2026"
title: Een datum converteren naar een string
---

{{< edit_this_page >}}

## Wat & Waarom?
Een datum omzetten naar een string betekent het transformeren van een datumobject naar leesbare tekst voor mensen. Programmeurs doen dit om data in begrijpelijke formaten weer te geven of om ze te serialiseren voor opslag en verzending.

## Hoe te:
In Clojure gebruiken we de Java interop-mogelijkheden om datums te formatteren. Hier is een snelle gids:

```clojure
(import java.text.SimpleDateFormat)
(import java.util.Date)

;; Maak een datumobject (laten we de huidige datum en tijd gebruiken)
(def now (Date.))

;; Stel het gewenste formaat in
(def formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

;; Formatteer de datum als een string
(def formatted-date (.format formatter now))

;; Print het uit
(println formatted-date)
;; Uitvoer zou kunnen zijn: "2023-03-15 09:26:45" (afhankelijk van de huidige datum en tijd)
```

## Diepgaande duik
Het converteren van datums naar strings is niet exclusief voor Clojure; het is een veelvoorkomende operatie in veel programmeertalen. Historisch gezien ontstond de behoefte hieraan al zodra computers begonnen met het verwerken van datums, omdat de leesbare representatie het begrip en de communicatie vergemakkelijkt, terwijl machines de voorkeur geven aan meer gestructureerde gegevensformaten.

In Clojure, omdat het draait op de Java Virtual Machine (JVM), maken we meestal gebruik van Java's datum- en tijd bibliotheken, zoals `java.util.Date` en `java.text.SimpleDateFormat`. Hoewel deze klassen lang bestaan, vertegenwoordigt het nieuwere `java.time` pakket (geïntroduceerd in Java 8) een alternatief met verbeterde thread-veiligheid en een intuïtievere API.

Clojure heeft geen ingebouwde datumformatteringsbibliotheek die deel uitmaakt van de kern van de taal, dus het is typisch om Java interop of externe bibliotheken te gebruiken, zoals `clj-time` (een wrapper rond Joda Time) voor meer idiomatische Clojure-oplossingen.

Hier is hoe je `java.time` zou kunnen gebruiken voor formattering:

```clojure
(import java.time.LocalDateTime)
(import java.time.format.DateTimeFormatter)

;; Maak een datumobject (de huidige datum en tijd)
(def now (LocalDateTime/now))

;; Stel het gewenste formaat in
(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

;; Formatteer de datum als een string
(def formatted-date (.format now formatter))

;; Print het uit
(println formatted-date)
;; Vergelijkbare uitvoer als eerder, met de huidige datum en tijd
```

Deze methode vermijdt de veranderlijkheid die aanwezig is bij SimpleDateFormat en moet de voorkeur hebben in nieuwe code waar thread-veiligheid een zorg is.

## Zie ook
- Java 8 Datum en Tijd gids: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- ClojureDocs, een door de gemeenschap aangedreven documentatie- en voorbeeldenrepository: [https://clojuredocs.org/](https://clojuredocs.org/)
- clj-time, een datum- en tijdbibliotheek voor Clojure: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
