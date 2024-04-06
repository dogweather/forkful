---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:19.863343-07:00
description: "Hoe: Clojure, een moderne dialect van Lisp, biedt Java-interoperabiliteit,\
  \ dus we gebruiken vaak Java's rijke Date en Time API. Historisch gezien werden\u2026"
lastmod: '2024-04-05T21:53:50.455895-06:00'
model: gpt-4-0125-preview
summary: Clojure, een moderne dialect van Lisp, biedt Java-interoperabiliteit, dus
  we gebruiken vaak Java's rijke Date en Time API.
title: Het huidige datum ophalen
weight: 29
---

## Hoe:
```Clojure
;; Java interop importeren om Date klassen te gebruiken
(import java.util.Date)
(import java.text.SimpleDateFormat)

;; De huidige datum en tijd krijgen
(defn current-date-time []
  (let [today (new Date)]
    (println "Huidige datum en tijd: " today)))

(current-date-time)
;; Uitvoer: Huidige datum en tijd:  Wed Apr 05 00:12:35 BST 2023

;; Datum formatteren naar een specifiek patroon
(defn formatted-current-date []
  (let [today (new Date)
        formatter (SimpleDateFormat. "dd-MM-yyyy")]
    (println "De datum van vandaag is: " (.format formatter today))))

(formatted-current-date)
;; Uitvoer: De datum van vandaag is:  05-04-2023
```

## Diepere duik
Clojure, een moderne dialect van Lisp, biedt Java-interoperabiliteit, dus we gebruiken vaak Java's rijke Date en Time API. Historisch gezien werden data heel anders behandeld - denk aan tandwielen en zonnewijzers – maar in programmering hadden we al vroeg Java's `Date` en `Calendar` in JDK 1.0. Nu hebben we ook `java.time` vanaf Java 8 voor een meer uitgebreide en verenigde aanpak van tijdsgegevens.

Hoewel `java.util.Date` goed werkt voor basisbehoeften, heeft het zijn eigenaardigheden, zoals veranderlijk zijn - wat betekent dat het kan veranderen na creatie, bijv. met `setTime`. `java.time` is onveranderlijk en veelzijdiger, maar voor eenvoudige taken zoals het pakken van de huidige datum, doet `Date` nog steeds de truc.

Alternatieven binnen Clojure zijn onder andere bibliotheken zoals clj-time, dat Joda Time inpakt (een voorloper van `java.time`), en tick, een moderne Clojure-bibliotheek voor omgaan met tijd. Elk heeft zijn voor- en nadelen afhankelijk van de reikwijdte en complexiteit van uw tijdbehandelingsbehoeften.

Wat betreft implementatie, is het ophalen van de huidige datum-tijd een eenvoudige kwestie in Clojure vanwege zijn Java-roots. Dat is doorgaans een regel code, hoewel datumformatting een paar meer stappen vereist en begrip van Java's datumformatteerpatronen en standaarden.

## Zie ook
Hier zijn enkele handige hoekjes van het web voor de nieuwsgierige Clojure tijdreiziger:

- Clojure Docs: https://clojuredocs.org/
- Java 8 Date/Time API: https://docs.oracle.com/javase/tutorial/datetime/
- clj-time GitHub-repo: https://github.com/clj-time/clj-time
- tick GitHub-repo: https://github.com/juxt/tick
