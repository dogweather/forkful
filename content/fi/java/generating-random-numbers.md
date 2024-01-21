---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:11.744863-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Arpakuutioita ei ole enää. Koodarit luovat satunnaislukuja ohjelmien varioimiseen ja esim. tietojen salaamiseen. Se tekee ohjelmista arvaamattomia ja mielenkiintoisia.

## How to: (Kuinka tehdä:)
```java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random random = new Random();

        // Satunnainen kokonaisluku välillä 0-99
        int randInt = random.nextInt(100);
        System.out.println("Satunnainen kokonaisluku: " + randInt);

        // Satunnainen double välillä 0.0-1.0
        double randDouble = random.nextDouble();
        System.out.println("Satunnainen double: " + randDouble);

        // Satunnainen long
        long randLong = random.nextLong();
        System.out.println("Satunnainen long: " + randLong);
    }
}
```
Esimerkkitulostus:
```
Satunnainen kokonaisluku: 45
Satunnainen double: 0.2324234231123123
Satunnainen long: 82374823743287432
```

## Deep Dive (Syväsukellus):
Random-luokka on vanha, osana Javaa versiosta 1.0. Uudempi ja suositeltu tapa on `SecureRandom` tietoturvasyistä, mutta se on hitaampi. Vielä toinen tapa on `ThreadLocalRandom` monisäikeisissä sovelluksissa. `Math.random()` on olemassa, mutta se vain kutsuu `Random.nextDouble()` alla.

Random-luvut eivät ole "oikeasti" satunnaisia. Ne ovat "pseudosatunnaisia", riippuen alkuluvusta (siemen). Jos siemen on sama, sarja on sama. Tietokoneiden täytyy käyttää monimutkaisia menetelmiä "oikeasti" satunnaisten siementen luomiseen, kuten hiiren liikkeet tai näppäimistön painallukset. 

## See Also (Lisätietoja):
- Oracle Java docs for `Random`: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Oracle Java docs for `SecureRandom`: https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html
- Stack Overflow discussion on random number generation: https://stackoverflow.com/questions/738629/math-random-versus-random-nextintint