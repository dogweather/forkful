---
title:                "Java: Satunnaislukujen tuottaminen"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi Satunnaisia Lukuja Generoidaan?

Satunnaiset luvut ovat erittäin hyödyllisiä monissa ohjelmoinnin konteksteissa. Niitä voidaan käyttää esimerkiksi satunnaisen käyttäjätunnuksen generoimiseen, arvontapeleihin tai tilastollisiin analyyseihin. Java-ohjelmoijana sinun kannattaa osata luoda satunnaisia lukuja voidaksesi toteuttaa monipuolisia ja tarkkoja ohjelmia.

## Miten Satunnaisia Lukuja Generoidaan?

Java tarjoaa valmiin Random-luokan, jota voidaan käyttää satunnaislukujen luomiseen. Ensimmäiseksi täytyy importata Random-luokka ja luoda uusi Random-olio. Tämän jälkeen voit kutsua Random-olion nextInt()-metodia, joka palauttaa satunnaisen kokonaisluvun. Seuraavassa koodiesimerkissä luodaan satunnainen luku väliltä 0-100 ja tulostetaan se konsoliin:

```Java
import java.util.Random;

public class SatunnaisetLuvut {
  public static void main(String[] args) {
    Random random = new Random();
    int luku = random.nextInt(101); // Pysyy välillä 0-100
    
    System.out.println("Satunnainen luku: " + luku);
  }
}
```

Tulostettu luku vaihtelee jokaisella ohjelman suorituskerralla. Voit myös muuttaa nextInt()-metodin parametria, jolloin satunnainen luku saa halutun välivalinnan.

## Syväsukellus Satunnaisiin Lukuihin

Random-luokassa on myös muita hyödyllisiä metodeja satunnaislukujen luomiseen. Esimerkiksi nextDouble()-metodi palauttaa satunnaisen desimaaliluvun välillä 0.0 ja 1.0. Voit myös asettaa Random-luokalle alkuseediksi jonkin arvon, jolloin satunnaisuus toistuu aina samassa järjestyksessä.

On myös hyvä huomata, että satunnaislukuja ei todellisuudessa voida generoida täysin satunnaisesti, vaan ne luodaan laskennallisesti perustuen alkuperäiseen alkuseediin. Siksi ei ole suositeltavaa käyttää satunnaisia lukuja esimerkiksi salausavainten luomiseen.

## Katso Myös

- [Oracle:n virallinen dokumentaatio Random-luokasta](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java-harjoitustehtäviä satunnaislukujen luomiseen](https://java-programming.mooc.fi/part-1/4-adding-functionality-to-programs/20-java-random-numbers)