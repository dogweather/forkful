---
title:    "Java: Lukujen generointi tietokoneohjelmoinnissa"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaislukujen tuottaminen on tärkeä osa monia ohjelmointitehtäviä, kuten pelien luomista ja tietojen muokkausta. Se voi myös auttaa simuloimaan ennustettua käyttäytymistä ja testaamaan koodin suorituskykyä. Yhteenvetona, satunnaislukujen generointi on tärkeä työkalu monelle Java-ohjelmoijalle.

## Näin

Satunnaislukujen tuottamiseen Java-kielellä on useita tapoja. Yksi vaihtoehto on käyttää Java Random-luokkaa, joka sisältää erilaisia ​​metodeja satunnaislukujen generoimiseen. Esimerkiksi seuraava koodinpätkä generoi satunnaisen kokonaisluvun väliltä 1-100 ja tulostaa sen konsoliin:

```Java
Random random = new Random();
int randomNumber = random.nextInt(100) + 1;
System.out.println("Satunnainen luku välillä 1-100: " + randomNumber);
```

Tulostus voisi olla esimerkiksi: "Satunnainen luku välillä 1-100: 74".

Lisäksi, jos haluat generoida satunnaisen liukuluvun, voit käyttää nextDouble() -metodia ja antaa sille parametrina haluamasi desimaalien tarkkuuden.

```Java
double randomDouble = random.nextDouble(0.001);
System.out.println("Satunnainen liukuluku kolmen desimaalin tarkkuudella: " + randomDouble);
```

Tulostus voisi olla esimerkiksi: "Satunnainen liukuluku kolmen desimaalin tarkkuudella: 0.542".

## Syvällisempi katsaus

On tärkeää muistaa, että vaikka satunnaisluvut voivat tuntua täysin sattumanvaraisilta, ne todellisuudessa tuotetaan tiettyä algoritmia käyttäen. Esimerkiksi Java Random-luokka käyttää "pseudo-satunnaisluku generaattoria", joka tuottaa samassa järjestyksessä samat luvut, jos sama "siemen" (seed) on annettu alkuperäisenä parametrina.

Tästä syystä, jos haluat todella satunnaisia lukuja, on suositeltavaa antaa jokaiselle ohjelman suoritukselle eri siemen. Voit tehdä tämän antamalla parametrina jokin vaihtuva arvo, esimerkiksi ajanhetki millisekunneissa:

```Java
long seed = System.currentTimeMillis();
Random random = new Random(seed);
```

Tämä lisää satunnaisuutta, mutta ei siltikään takaa täysin sattumanvaraisia lukuja.

## Katso myös

- [Java Random-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Satunnaislukujen generointi Java-alustalla -opas](https://www.baeldung.com/java-generate-random-long-float-integer-double)
- [Satunnaislukujen tuottaminen JavaFX-sovelluksessa](https://www.tutorialspoint.com/generating-random-numbers-in-javafx)