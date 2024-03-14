---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:16.808680-07:00
description: "Het berekenen van toekomstige of verleden datums betreft het manipuleren\
  \ van datums om erachter te komen wat ze zullen zijn na een bepaalde periode of\
  \ wat\u2026"
lastmod: '2024-03-13T22:44:50.433663-06:00'
model: gpt-4-0125-preview
summary: "Het berekenen van toekomstige of verleden datums betreft het manipuleren\
  \ van datums om erachter te komen wat ze zullen zijn na een bepaalde periode of\
  \ wat\u2026"
title: Een datum in de toekomst of het verleden berekenen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het berekenen van toekomstige of verleden datums betreft het manipuleren van datums om erachter te komen wat ze zullen zijn na een bepaalde periode of wat ze waren. Programmeurs doen dit voor zaken zoals het plannen van evenementen, herinneringen, of het uitvogelen van vervaldatums.

## Hoe:

In Clojure gebruik je voornamelijk de `clj-time` bibliotheek voor datumoperaties. Hier is een snelle demonstratie:

```clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])
(require '[clj-time.periodic :as periodic])

;; Voeg 5 dagen toe aan de huidige datum
(let [now (time/now)
      five-days (time/plus now (time/days 5))]
  (str "Vijf dagen vanaf nu: " (coerce/to-string five-days)))

;; Trek 10 dagen af van een specifieke datum
(let [specific-date (coerce/to-date-time "2023-03-01T12:00:00.000Z")
      ten-days-ago (time/minus specific-date (time/days 10))]
  (str "Tien dagen voor 1 maart 2023: " (coerce/to-string ten-days-ago)))
```

Voorbeelduitvoer:
```
"Vijf dagen vanaf nu: 2023-03-23T08:00:00.000Z"
"Tien dagen voor 1 maart 2023: 2023-02-19T12:00:00.000Z"
```

## Diepgaande Duik

In de beginjaren gebruikten programmeurs Java's `Date` en `Calendar` klassen. Maar laten we eerlijk zijn, ze zijn een hoofdpijn— breedsprakig en foutgevoelig. De `clj-time` bibliotheek bracht wat gezond verstand, door Joda-Time's meer ontwikkelaar-vriendelijke API in te pakken.

Alternatieven? Java 8 introduceerde `java.time` (JSR-310), wat vrij goed is, maar in Clojure's wereldje voelen we ons nog steeds comfortabel met `clj-time`.

Bij het berekenen van datums gebruik je periodes voor concepten als "dagen" en "maanden" en duuraties voor nauwkeurige milliseconde-tellingen. Houd rekening met tijdzones—datums en tijden kunnen drastisch verschuiven afhankelijk van de tijdzone-regels, en zomertijd (DST) kan roet in het eten gooien.

## Zie Ook

- `clj-time` GitHub repo: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Clojure’s `java-time`: [https://github.com/dm3/clojure.java-time](https://github.com/dm3/clojure.java-time)
