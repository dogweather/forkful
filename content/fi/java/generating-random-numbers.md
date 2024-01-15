---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "Java: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Monissa Java-ohjelmissa tarvitaan satunnaisia numeroita esimerkiksi pelilogiikassa, testauksessa tai tietojen generoinnissa.

## Kuinka tehdä

```Java
// Java Random-luokkaa käyttäen

import java.util.Random;

public class RandomNumbers {
    public static void main(String[] args) {

        // Luo uusi Random-objekti
        Random random = new Random();

        // Satunnaisen kokonaisluvun generointi väliltä 1-10
        int randomInt = random.nextInt(10) + 1;
        System.out.println("Satunnainen kokonaisluku: " + randomInt);

        // Satunnaisen liukuluvun generointi väliltä 0.0-1.0
        double randomDouble = random.nextDouble();
        System.out.println("Satunnainen liukuluku: " + randomDouble);
    }
}
```

```Java
// Java Math-luokkaa käyttäen

public class RandomNumbers {
    public static void main(String[] args) {

        // Satunnaisen kokonaisluvun generointi väliltä 1-10
        int randomInt = (int)(Math.random() * 10) + 1;
        System.out.println("Satunnainen kokonaisluku: " + randomInt);

        // Satunnaisen liukuluvun generointi väliltä 0.0-1.0
        double randomDouble = Math.random();
        System.out.println("Satunnainen liukuluku: " + randomDouble);
    }
}
```

Esimerkkilähtö:

> Satunnainen kokonaisluku: 7
>
> Satunnainen liukuluku: 0.8708039083763881

## Syvempi sukellus

Java tarjoaa sekä Random- että Math-luokan avulla mahdollisuuden generoida satunnaisia numeroita. Random-luokkaa käyttämällä voimme luoda uuden objektin, joka tarjoaa erilaisia metodeja satunnaisen numeron luomiseen. Math-luokka tarjoaa puolestaan staattisia metodeja, joiden avulla satunnainen luku voidaan generoida ilman erillistä objektia.

Satunnaisien numeroiden luomisessa on tärkeä ottaa huomioon, että ne eivät ole täysin satunnaisia vaan ennalta määrättyjen kaavojen ja algoritmien tulosta. Siksi niitä ei tulisi käyttää tietoturvasovelluksissa.

## Katso myös

- [Java Random-luokka](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java Math-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)