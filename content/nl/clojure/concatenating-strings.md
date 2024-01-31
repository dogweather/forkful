---
title:                "Samenvoegen van strings"
date:                  2024-01-28T21:56:47.250482-07:00
model:                 gpt-4-0125-preview
simple_title:         "Samenvoegen van strings"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
