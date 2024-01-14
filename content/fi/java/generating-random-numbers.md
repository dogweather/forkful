---
title:                "Java: Sattumanvaraisten numeroiden luominen"
simple_title:         "Sattumanvaraisten numeroiden luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi - satunnaislukujen tuottaminen Java-ohjelmoinnissa

Satunnaislukujen tuottaminen on tärkeä osa Java-ohjelmointia monessa eri käyttötarkoituksessa. Satunnaislukuja käytetään esimerkiksi tietokonepelien, arvontojen ja simulaatioiden luomisessa. Ne voivat myös olla hyödyllisiä testauksessa ja salausalgoritmeissa. Tässä oppaassa käymme läpi, miten voit luoda satunnaislukuja Java-koodilla.

## Kuinka - esimerkkejä koodista ja tulostuksista

Satunnaislukujen generoimiseen Java-koodissa on useita erilaisia tapoja. Yksi yleisimmistä on käyttää Math-luokan random-metodeita. Ne palauttavat double-tyyppisen satunnaisluvun välillä 0.0 ja 1.0. Voit myös määrittää haluamasi ala- ja ylärajan käyttämällä Math.random() metodia yhdessä Math.floor-metodin kanssa, joka pyöristää luvun alaspäin.

```Java
// Palauttaa satunnaisen luvun väliltä 0.0 ja 1.0
double luku = Math.random();

// Palauttaa satunnaisen luvun väliltä 1 ja 10
int luku = (int) (Math.random() * 10) + 1;

// Palauttaa satunnaisen luvun väliltä 5 ja 15
int luku = (int) (Math.floor(Math.random() * 11) + 5);
```

Toinen tapa luoda satunnaislukuja on käyttää Random-luokkaa. Sen avulla voidaan määrittää myös muun tyyppisiä lukuja, kuten kokonais- ja liukulukuja. Alla on esimerkki, miten voidaan luoda lista satunnaisia kokonaislukuja väliltä 1 ja 100.

```Java
Random random = new Random();

// Luodaan 5 satunnaista kokonaislukua väliltä 1 ja 100
for (int i = 0; i < 5; i++) {
    int luku = random.nextInt(100) + 1;
    System.out.println(luku);
}

// Tulostus voi olla esimerkiksi:
// 67
// 24
// 89
// 43
// 12
```

## Syvempi sukellus - lisätietoa satunnaislukujen generoinnista

On tärkeää huomata, että satunnaislukujen generoiminen Java-koodissa ei ole täysin sattumanvaraista. Generoitu luku perustuu aina annettuun siemenarvoon, joka määritetään joko automaattisesti tai käyttäjän toimesta. Jos käyttää samaa siemenarvoa, saadaan aina sama satunnaisluku. Tämä voi olla hyödyllistä testauksessa, mutta esimerkiksi salausalgoritmeissa halutaan käyttää mahdollisimman satunnaista siemenarvoa.

On myös tärkeää muistaa, että satunnaislukuja luodessa tärkeä osa on niiden jakautuminen. Esimerkiksi jos haluat generoida satunnaislukuja, jotka noudattavat normaalijakaumaa, voit käyttää apuna Random ja Gaussian-metodeita. Tämä tarkoittaa, että generoidut luvut ovat todennäköisesti lähempänä keskiarvoa, ja harvemmin poikkeavat reilusti siitä.

## Katso myös

- [Java Random Class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Generating Random Numbers in Java](https://www.baeldung.com/java-generating-random-numbers)
- [Math Class](