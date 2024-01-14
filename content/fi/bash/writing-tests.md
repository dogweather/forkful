---
title:                "Bash: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa testejä
Testien kirjoittaminen on tärkeä osa ohjelmoinnin prosessia, sillä se varmistaa koodin toimivuuden ja ehkäisee mahdollisten virheiden syntymistä. Näin säästetään aikaa ja vaivaa pitkällä tähtäimellä, sillä hyvin testattu koodi vaatii vähemmän korjauksia ja huoltoa myöhemmin.

## Kuinka kirjoittaa testejä
Testien kirjoittaminen Bash-kielen avulla on helppoa ja tehokasta. Käytämme testikirjastoja, kuten Batsia ja shunit2:ta, joiden avulla voimme luoda monipuolisia ja luotettavia testejä. Alla on esimerkki Batsin käytöstä testin luomiseen:

```Bash
@test "Funktion summa oikein" {
  run summa 1 2
  [ "$status" -eq 0 ]
  [ "$output" -eq 3 ]
}
```

Tässä esimerkissä luomme testin, joka varmistaa, että funktio "summa" antaa oikean tuloksen parametreille 1 ja 2. "Run" -komento suorittaa funktion ja tallentaa sen palauttaman statuksen muuttujaan "$status" ja tulosteen "$output" muuttujaan. Tämän jälkeen tarkistamme, onko status 0 ja tulos 3, jotka olisi odotettu tulos.

## Syvällisempi sukellus testien kirjoittamiseen
Testien kirjoittaminen auttaa meitä luomaan luotettavaa koodia ja säästämään aikaa ja vaivaa korjauksilta. Hyvät testit kattavat kaikki mahdolliset skenaariot ja varmistavat, että koodi toimii odotetulla tavalla. Testejä voi myös automatisoida osaksi ohjelmointiprosessia, mikä helpottaa ja nopeuttaa ohjelmien huoltoa.

Testien kirjoittamisen lisäksi on tärkeää myös ymmärtää, miten testien avulla voi parantaa koodin laatua ja optimoida suorituskykyä. Näitä tekniikoita ja vinkkejä voi löytää esimerkiksi tutorial-sivustoilta ja ohjelmointiyhteisöistä.

## Katso myös
- [Bats-testikirjaston dokumentaatio](https://github.com/bats-core/bats-core)
- [shunit2-testikirjaston dokumentaatio](https://github.com/kward/shunit2)
- [Ohjelmiston testaus -opetusohjelma (Helsingin yliopisto)](https://www.cs.helsinki.fi/uudet-oppijat/ohjelmointi/opetusohjelma/program-and-how-learn-tests-testing)
- [Bash-yhteisö foorumi ja keskusteluryhmä](https://www.bash.org/)