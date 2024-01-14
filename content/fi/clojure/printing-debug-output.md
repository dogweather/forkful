---
title:                "Clojure: Vianmäärityksen tulostaminen"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi?

On monia syitä, miksi kannattaa tulostaa debug-tulosteita ohjelmoinnin aikana. Yksi tärkeimmistä syistä on virheiden etsiminen ja ohjelman toiminnan ymmärtäminen. Tulostamalla tietoja koodin suorituksen eri vaiheista, voit tarkastella mitä tapahtuu ja missä vaiheessa mahdollinen virhe ilmenee.

## Miten?

Debug-tulosteiden tulostaminen on helppoa Clojure-ohjelmoinnissa. Voit käyttää funktiota `println` ja antaa sille haluamasi tiedon tulostettavaksi. Alla on esimerkki koodista:

```Clojure
(def num1 5)
(def num2 10)
(println "Summa:" (+ num1 num2))
```

Tämän koodin tuloste on `Summa: 15`. Voit myös tulostaa esimerkiksi muuttujien arvoja tai ohjelman suorituksen eri vaiheissa olevien funktioiden palauttamaa tietoa. Debug-tulosteita voi myös käyttää vertailemaan esimerkiksi odotettuja ja todellisia tuloksia, ja näin havaita virheitä ohjelmassa.

## Syvemmälle

Debug-tulosteiden tulostaminen voi myös auttaa kehittämään ymmärrystäsi ohjelmoinnista ja Clojure-kielen toiminnasta. Voit tarkastella tulosteita ja vertailla niitä koodiisi, jolloin voit oppia uusia asioita ja löytää uusia tapoja ratkaista ongelmia.

On myös tärkeää muistaa poistaa tai kommentoida debug-tulosteet ennen kuin julkaiset lopullisen version ohjelmastasi, jotta ohjelma toimii suorituskykyisemmin.

## Katso myös

- [Clojure - Debugging](https://cljdoc.org/d/clojurians-org/common-clojure/0.2.24/doc/debugging)
- [Debugging Clojure with Reveal](https://cognitect.com/blog/2014/1/20/debugging-clojure-with-revel)
- [Clojure Debugging Cheatsheet](https://github.com/essential-logic/Clojure-Debugging-Cheatsheet/blob/master/Clojure-Debugging-Cheatsheet.pdf)

Kiitos lukemisesta, toivottavasti tämä auttoi sinua ymmärtämään debug-tulosteiden käyttöä Clojure-ohjelmoinnissa. Muista käyttää niitä hyödyksesi ratkaislessasi ongelmia ohjelmassasi!