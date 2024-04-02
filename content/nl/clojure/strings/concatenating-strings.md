---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:47.250482-07:00
description: "Stringconcatenatie betekent het eind-aan-eind samenvoegen van strings\
  \ - \"hallo\" + \"wereld\" wordt \"hallowereld\". Programmeurs doen dit om tekst\
  \ samen te\u2026"
lastmod: '2024-03-13T22:44:50.410632-06:00'
model: gpt-4-0125-preview
summary: "Stringconcatenatie betekent het eind-aan-eind samenvoegen van strings -\
  \ \"hallo\" + \"wereld\" wordt \"hallowereld\". Programmeurs doen dit om tekst samen\
  \ te\u2026"
title: Samenvoegen van strings
weight: 3
---

## Wat & Waarom?

Stringconcatenatie betekent het eind-aan-eind samenvoegen van strings - "hallo" + "wereld" wordt "hallowereld". Programmeurs doen dit om tekst samen te stellen, zoals URL's, berichten of resultaten op basis van gebruikersinvoer of programmadata.

## Hoe te:

Clojure maakt stringconcatenatie eenvoudig met de `str` functie. Laten we er direct induiken:

```clojure
;; Eenvoudige concatenatie met de str-functie
(str "Hallo, " "wereld!")
;; => "Hallo, wereld!"

;; Meerdere strings samenvoegen
(str "Clojure" " is" " geweldig!")
;; => "Clojure is geweldig!"

;; Strings en andere waarden combineren
(str "Het antwoord is " 42)
;; => "Het antwoord is 42"

;; Gebruik maken van apply om een reeks strings samen te voegen
(apply str ["Voeg" " " "deze" " " "strings samen!"])
;; => "Voeg deze strings samen!"
```

Geweldig, dus je hebt het in actie gezien. Onthoud gewoon dat `str` werkt met elke waarde door `toString` erop aan te roepen. Als het nil is, krijg je de string "nil".

## Diepere Duik

Historisch gezien is stringconcatenatie er al sinds we tekst programmatisch moesten verwerken, en elke taal biedt zijn eigen methoden. In Clojure is `str` deel van de kernbibliotheek, geïntroduceerd voor eenvoud en uniformiteit.

Alternatieven voor `str`? Ja! `StringBuilder` kan efficiënter zijn voor veel concatenaties, vooral in lussen. Clojure kan Java-methoden aanroepen, dus je kunt ook `StringBuilder` gebruiken:

```clojure
;; StringBuilder gebruiken voor efficiëntie
(let [builder (StringBuilder.)]
  (.append builder "Dit is")
  (.append builder " een meer")
  (.append builder " efficiënte manier!")
  (.toString builder))
;; => "Dit is een meer efficiënte manier!"
```

Waarom niet altijd `StringBuilder` gebruiken dan? Voor de meeste alledaagse taken is `str` simpeler en snel genoeg. `StringBuilder` excelleert in scenario's met hoge prestaties met veel concatenaties.

Wat implementatie betreft, aangezien Clojure gehost is op de JVM, profiteert het van Java's mogelijkheden voor stringverwerking. Echter, zoals bij Java `String`s, creëert elke `str` aanroep een nieuwe `String`, wat een overweging voor het geheugen kan zijn.

## Zie Ook

- Documentatie van Clojure's `str` functie: [Clojure Strings](https://clojuredocs.org/clojure.core/str)
- Java's `StringBuilder`: [Documentatie van StringBuilder](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- Praktische gids voor Clojure over `str` en meer: [Clojure voor de moedigen en echten](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
