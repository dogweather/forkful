---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:17.527597-07:00
description: 'Hoe: Om naar de standaardfout in Clojure te schrijven, gebruik je `binding`
  met `*err*`. Hier is een snel voorbeeld.'
lastmod: '2024-03-13T22:44:50.436553-06:00'
model: gpt-4-0125-preview
summary: Om naar de standaardfout in Clojure te schrijven, gebruik je `binding` met
  `*err*`.
title: Schrijven naar standaardfout
weight: 25
---

## Hoe:
Om naar de standaardfout in Clojure te schrijven, gebruik je `binding` met `*err*`. Hier is een snel voorbeeld:

```Clojure
(binding [*err* *out*]
  (println "Dit gaat naar de standaardfout"))
```

Voorbeeld van output (in je shell):

```
$ clj your_script.clj 2> error.log
$ cat error.log
Dit gaat naar de standaardfout
```

Dit fragment bindt `*err*` aan `*out*`, wat de standaarduitvoer is, zodat je kunt zien wat typisch naar `stderr` zou gaan.

## Diepere Duik
Historisch gezien hadden Unix-systemen twee afzonderlijke uitvoerstromen, `stdout` en `stderr`, voor verschillende datatypes. In Clojure verwijst `*out*` naar `stdout` en `*err*` naar `stderr`. Alternatieven voor `binding` omvatten het direct gebruiken van Java interop (bv. `(.println System/err "bericht")`). Wat betreft implementatie, `*err*` is een dynamische var, wat toelaat voor thread-lokale bindingen — een nuance die kan beïnvloeden hoe fouten worden gelogd in gelijktijdige applicaties.

## Zie Ook
- Clojure Docs over `*err*`: https://clojuredocs.org/clojure.core/*err*
- Clojure Docs over `binding`: https://clojuredocs.org/clojure.core/binding
- Java API voor `PrintStream` (waar `System/err` een voorbeeld van is): https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html

Voor een breder begrip van standaardstromen kunnen de volgende bronnen ook nuttig zijn:
- Wikipedia over Standaardstromen: https://nl.wikipedia.org/wiki/Standaardstromen
- De Unix standaardstromen documentatie: `man stdio`
