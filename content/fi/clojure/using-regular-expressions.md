---
title:                "Clojure: Säännöllisten lausekkeiden käyttö"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllinen työkalu tekstien käsittelyyn ja muokkaamiseen. Niiden avulla voit etsiä ja korvata tiettyjä merkkijonoja tai löytää tietynlaista rakennetta sisältäviä tekstinpätkiä. Ne auttavat myös validoimaan syötteitä ja tekemään monimutkaisia hakuja tekstimassoista. Säännöllisiä lausekkeita käytetään laajasti ohjelmoinnissa ja tekstinkäsittelysovelluksissa, joten on hyödyllistä oppia niiden käyttöä.

## Kuinka käyttää säännöllisiä lausekkeita?

Säännöllisten lausekkeiden käyttö Clojurella on melko suoraviivaista. Yksinkertaisimmassa tapauksessa voit käyttää ```re-find```-funktiota löytääksesi tietyn mittaisen merkkijonon, esimerkiksi:

```Clojure
(re-find #"abc" "abcd") ; => "abc"
(re-find #"123" "abc")  ; => nil
```

Voit myös käyttää säännöllisiä lausekkeita muuttamaan tekstimuotoja helpommin. Esimerkiksi, voit käyttää ```re-pattern```-funktiota luomaan säännöllisen lausekkeen, jolla voi hakea kaikki 3-merkkiset sanat, ja sitten käyttää ```clojure.string/replace```-funktiota korvaamaan ne haluamallasi merkkijonolla. Esimerkiksi:

```Clojure
(->> "moi hei moi moi"
     (re-pattern #"\w{3}")
     (clojure.string/replace "lol")) ; => "lol lol lol lol"
```

Jos haluat tehdä monimutkaisempia hakuja, voit käyttää erilaisia säännöllisten lausekkeiden ominaisuuksia, kuten vaihtoehtoisia haaroja, toistoja ja ryhmiä. Käy läpi Clojuren dokumentaatio löytääksesi kaikki käytettävissä olevat toiminnot ja ominaisuudet.

## Syvempää tietoa säännöllisistä lausekkeista

Vaikka säännöllisten lausekkeiden avulla voit tehdä monimutkaisia hakuja ja muokkauksia, niiden käyttö voi olla joskus haastavaa ja vaikeasti luettavaa. On tärkeää muistaa käyttää selkeitä ja hyvin dokumentoituja säännöllisiä lausekkeita pitkän ajan projekteissa. Lisäksi, jos käytät säännöllisiä lausekkeita usein, voit tallentaa niitä muuttujaan ja käyttää niitä myöhemmin uudelleen, mikä helpottaa ohjelmien ylläpitoa.

## Katso myös

- Clojuren virallinen säännöllisiä lausekkeita käsittelevä dokumentaatio: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace
- Säännöllisten lausekkeiden opetusohjelma: https://regexone.com/lesson/introduction_abcs
- Regular Expressions Cheat Sheet: https://www.rexegg.com/regex-quickstart.html