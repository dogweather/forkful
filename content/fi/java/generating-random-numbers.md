---
title:    "Java: Satunnaislukujen generointi"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Random-numerogeneraattorien luominen voi olla tärkeää, kun haluat lisätä satunnaisuutta ohjelmiisi tai simuloida erilaisia tapahtumia. Se voi myös olla hyödyllistä peleissä tai tietoturvatestauksessa.

## Kuinka

```Java
// Luo uusi Random-objekti
Random r = new Random();

// Luo satunnainen kokonaisluku väliltä 0-10
int randomNumber = r.nextInt(11);

// Tulosta luku konsoliin
System.out.println("Satunnainen luku on: " + randomNumber);
```

Tässä esimerkissä käytämme Random-luokkaa luomaan uuden objektin, jolla voimme generoida satunnaisia lukuja. Käytämme sitten `nextInt()` -funktiota asettaaksemme välittämämme arvon, tässä tapauksessa 11, joka tarkoittaa, että haluamme generoida lukuja välillä 0-10.

Voit myös käyttää `nextDouble()` -funktiota, joka generoi satunnaisia desimaalilukuja välillä 0.0-1.0.

## Syventyminen

Random-numerogeneraattorien taustalla on matematiikka ja todennäköisyyslaskenta. Generoidessaan "satunnaisia" lukuja, nämä generaattorit käyttävät matemaattisia algoritmeja ja siemenarvoja luodakseen lukuja, jotka näyttävät satunnaisilta. Tämän vuoksi todellisuudessa nämä numerot eivät ole täysin satunnaisia, mutta ne ovat tarpeeksi sattumanvaraisia, jotta voidaan käyttää erilaisiin tarkoituksiin.

On myös tärkeää muistaa, että nämä generaattorit ovat ennustettavissa. Jos sama siemenarvo annetaan, sama sarja lukuja generoidaan aina. Tästä syystä on tärkeää käyttää erilaisia siemenarvoja tai jopa ajan perusteella vaihtuvia siemeniä, jotta saadaan todella satunnaisia lukuja.

## Katso myös

- [Random-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Miksi sinun ei pitäisi käyttää Math.random() -funktiota Java:ssa](https://stackoverflow.com/questions/738629/math-random-versus-java-util-random-nextint)
- [Matematiikkataustaa random-numerogeneraattoreille](https://random.org/randomness/)