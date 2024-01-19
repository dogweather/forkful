---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Satunnaislukujen tuottaminen Java-ohjelmassa tarkoittaa ennakoimattomien numeroiden luomista. Näitä lukuja käytetään mm. pelaamiseen, kryptografiaan ja tilastolliseen mallintamiseen.  

## Näin teet:

Tässä on yksinkertainen esimerkki satunnaisluvun generoimisesta Javassa:
```Java
import java.util.Random; //luokan tuonti

public class Main {
    public static void main(String[] args) {
        Random rand = new Random(); //luodaan Random-olio

        int rand_int1 = rand.nextInt(1000); //tuotetaan satunnaisluku välillä 0-999
        System.out.println("Satunnaisluku on : "+rand_int1);  //tulostetaan luku
    }
}
```

Esimerkkiohjelman suorituksen tulostus voisi olla esimerkiksi:
```
Satunnaisluku on : 657
```

## Syväsyöksy:

Satunnaislukujen generointi on ollut osa ohjelmointia jo pitkään, sillä tietokoneiden alkuaikoina ei ole ollut helppoa tuottaa todellisia satunnaislukuja. Javan java.util.Random-luokka toimii pseudosatunnaislukugeneraattorina ja se on ollut käytössä Javan alkuvuosista asti. Sen lisäksi Java tarjoaa java.util.concurrent.ThreadLocalRandom-luokan ja java.security.SecureRandom-luokan, jotka tarjoavat myös erilaisia tapoja generoida satunnaislukuja.

## Katso myös:

- Oracle Java API:n määritelmä java.util.Random-luokasta: [Linkki](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- Java Tutorial satunnaislukujen generoinnista: [Linkki](https://docs.oracle.com/javase/tutorial/essential/concurrency/newrandom.html)
- Stack Overflow keskustelu: Eroja java.util.Random ja java.util.concurrent.ThreadLocalRandom välillä: [Linkki](https://stackoverflow.com/questions/363681/how-do-i-generate-random-integers-within-a-specific-range-in-java)