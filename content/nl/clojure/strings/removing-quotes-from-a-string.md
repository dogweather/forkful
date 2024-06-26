---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:24.726457-07:00
description: "Hoe: In Clojure zijn strings onveranderlijk, dus als we het hebben over\
  \ \"het verwijderen van aanhalingstekens\", hebben we het eigenlijk over het cre\xEB\
  ren\u2026"
lastmod: '2024-03-13T22:44:50.406453-06:00'
model: gpt-4-0125-preview
summary: "In Clojure zijn strings onveranderlijk, dus als we het hebben over \"het\
  \ verwijderen van aanhalingstekens\", hebben we het eigenlijk over het cre\xEBren\
  \ van een nieuwe string zonder aanhalingstekens."
title: Quotes verwijderen uit een string
weight: 9
---

## Hoe:
In Clojure zijn strings onveranderlijk, dus als we het hebben over "het verwijderen van aanhalingstekens", hebben we het eigenlijk over het creëren van een nieuwe string zonder aanhalingstekens. Hier is de essentie met behulp van `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Laten we die dubbele aanhalingstekens weggooien
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; En de enkele aanhalingstekens eruit schoppen
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Voorbeeldgebruik:
(remove-double-quotes "\"Hallo, Wereld!\"") ; => "Hallo, Wereld!"
(remove-single-quotes "'Hallo, Wereld!'")   ; => "Hallo, Wereld!"
```
Wil je zowel enkele als dubbele aanhalingstekens in één klap aanpakken? Check dit:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Voorbeeldgebruik:
(remove-quotes "\"Hallo, 'Clojure' Wereld!\"") ; => "Hallo, Clojure Wereld!"
```

## Diepere Duik
In de tijd dat data rommeliger was dan een kinderkamer, waren aanhalingstekens in strings de norm voor het aanduiden van tekst. Maar naarmate de informatica evolueerde, werden aanhalingstekens meer dan alleen tekstafscheiders - ze kregen syntactische rollen in programmeertalen.

Clojure, met zijn Lisp-erfgoed, gebruikt aanhalingstekens niet op dezelfde manier als sommige andere talen dat zouden kunnen. Ze worden zeker gebruikt voor het aanduiden van strings, maar ze hebben ook een speciale rol bij het creëren van literals. Desondanks blijft het verwijderen van aanhalingstekens uit strings een tijdloze taak.

Waarom niet gewoon de uiteinden van een string afsnijden? Nou, dat gaat ervan uit dat je aanhalingstekens altijd het begin en einde van je string omhelzen als een paar over-affectieve grootouders. Data in de echte wereld is rommeliger. Voer regex (reguliere expressies) in, waarmee je die aanhalingstekens kunt richten, waar ze ook verstopt zitten.

Alternatieven? Zeker, je kunt fancy worden met `subs`, `trim`, `triml`, `trimr`, of zelfs transducers als je wilt opscheppen. Maar `replace` met regex is als het brengen van een lichtzwaard naar een mesgevecht - het snijdt recht door naar de kern.

## Zie Ook
Als je brein jeukt voor meer Clojure string manipulatie goedheid, kunnen deze broodkruimels helpen:

- ClojureDocs over `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Reguliere expressies in Clojure: https://clojure.org/guides/learn/syntax#_regex
- Java interop voor stringverwerking (Clojure draait tenslotte op de JVM): https://clojure.org/reference/java_interop#_working_with_strings

Stop niet bij het verwijderen van aanhalingstekens. Er is een hele wereld van stringmagie daarbuiten in Clojure-land die wacht om ontdekt te worden.
