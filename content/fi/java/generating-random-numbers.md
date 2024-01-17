---
title:                "Satunnaislukujen luominen"
html_title:           "Java: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Satunnaislukujen generointi tarkoittaa satunnaisten numeroiden luomista ohjelmassa. Ohjelmoijat tekevät tätä esimerkiksi testauksen, pelien tai salausalgoritmien toteuttamiseksi.

## Kuinka tehdä:

Java tarjoaa valmiin luokan nimeltä Random, joka sisältää metodeja satunnaislukujen generoimiseksi. Esimerkiksi:

```Java 
Random random = new Random();

// Generoi kokonaisluku väliltä 0-9
int randomInt = random.nextInt(10);
System.out.println(randomInt);
```

Tämä koodi tulostaisi esimerkiksi luvun 5.

## Syväsukellus:

Satunnaislukujen generoinnilla on pitkä historia matematiikassa ja tietojenkäsittelyssä. Aikaisemmin satunnaislukuja saatiin esimerkiksi arpakuutioita heittämällä, mutta nykyään tietokoneohjelmat tarjoavat tarkempia ja nopeampia tapoja generoida satunnaisia lukuja.

Java tarjoaa myös muita vaihtoehtoja satunnaislukujen generointiin, kuten ThreadLocalRandom-luokan, joka on suunniteltu monisäikeisissä ympäristöissä toimiville sovelluksille.

Satunnaislukujen generoiminen perustuu matemaattisiin algoritmeihin, jotka tuottavat luvut, jotka näyttävät satunnaisilta mutta ovat todellisuudessa ennustettavissa. Tämä on tärkeää ottaa huomioon esimerkiksi salausalgoritmeja suunnitellessa.

## Katso myös:

- [Oracle: Random-luokan dokumentaatio (englanniksi)](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Oracle: ThreadLocalRandom-luokan dokumentaatio (englanniksi)](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)