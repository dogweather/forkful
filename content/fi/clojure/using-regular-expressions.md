---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Clojure: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllisiä apuvälineitä koodauksessa, sillä ne mahdollistavat tiettyjen merkkijonojen löytämisen ja käsittelyn tekstissä. Ne ovat erityisen käteviä tekstien etsimisessä ja muokkaamisessa, ja voivat säästää aikaa ja vaivaa monimutkaisten etsintä- ja korvaustoimintojen suorittamisessa.

## Kuinka käyttää säännöllisiä lausekkeita Clojurella

Säännöllisiä lausekkeita voi käyttää Clojurella eri tavoin, mutta yleisimmin niitä käytetään "re-seq" ja "re-find" funktioiden yhteydessä. "re-seq" palauttaa listan kaikista tekstissä esiintyvistä merkkijonoista, jotka vastaavat määriteltyä säännöllistä lauseketta. "re-find" puolestaan palauttaa ensimmäisen löydetyn vastaavuuden. Käytännön esimerkki näyttää kuinka etsiä ja korvata kaikki ensimmäisen b-kirjaimen jälkeen tulevat h-kirjaimet merkkijonossa:

```Clojure
(def s "abcde fghi jklmn")
(re-find #"b[\w]*h" s) ; palauttaa bde
(re-seq #"b[\w]*h" s) ; palauttaa listan [bde, fgh]
(re-replace #"b[\w]*h" s "replaced") ; palauttaa "areplaced fghi jklmn"
```

## Syvempi sukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet perustuvat usein merkkijonojen erilaisiin järjestelmiin ja merkistöihin. Ne ovat myös vahvasti käytettyjä tiedonkäsittelyssä ja tiedonlouhinnassa. Clojurella on käytössä Java Regular Expression Syntax, joten Java:n dokumentaatiosta ja esimerkeistä voi olla hyötyä ymmärtämiseen. Säännöllisiä lausekkeita voi myös laajentaa monimutkaisemmaksi käyttämällä esimerkiksi kvantifikaattoreita, ryhmittelemistä ja toistolausekkeita.

## Katso myös

- [Java säännöllinen lausekeopas](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Clojure re-seq dokumentaatio](https://clojuredocs.org/clojure.core/re-seq)
- [Clojure re-find dokumentaatio](https://clojuredocs.org/clojure.core/re-find)