---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Clojure: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miks
Miksi joku ylipäätään haluaisi yhdistellä merkkijonoja? On olemassa monia tilanteita, joissa tarvitaan dynaamista sisältöä tai muokattavia viestejä, kuten esimerkiksi verkkosivujen rakentamisessa tai automatisoiduissa viesteissä. Yhdistämällä merkkijonoja, pystyt luomaan joustavia ratkaisuja tällaisiin tarpeisiin.

## Näin
Yhdistäminen tapahtuu Clojuren `.concat`-funktion avulla. Tässä esimerkissä luodaan uusi merkkijono yhdistämällä kolme erillistä merkkijonoa.

```Clojure
(let [string1 "Tämä on "
      string2 "yksi merkkijono, "
      string3 "joka yhdistetään"]
  (str string1 string2 string3))
```

Tuloksena saadaan merkkijono "Tämä on yksi merkkijono, joka yhdistetään". Huomaa, että `str`-funktio yhdistää automaattisesti annetut arvot merkkijonoiksi.

## Syvemmälle
Clojuren merkkijonojen yhdistäminen on tehokasta ja suorituskykyistä. Tämä johtuu siitä, että merkkijonot ovat muuttumattomia tietorakenteita, mikä tarkoittaa sitä, että tallennetun merkkijonon sisältöä ei voida muuttaa. Tämä nopeuttaa ohjelman suoritusta, kun merkkijonoja yhdistellään, sillä ei tarvitse huolehtia mahdollisista muutoksista.

Voit myös käyttää `.concat`-funktion sijaan `str`-funktiota, joka yhdistää merkkijonot tehokkaammin ja tarjoaa lisää mahdollisuuksia dynaamisiin ratkaisuihin.

## Katso myös
- [Clojure opetusohjelma](https://clojure.org/guides/learn/syntax)
- [Clojure merkkijonon toimenpiteet](https://clojuredocs.org/clojure.string/replace)