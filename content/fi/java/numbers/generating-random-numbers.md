---
title:                "Satunnaislukujen generointi"
aliases:
- /fi/java/generating-random-numbers/
date:                  2024-01-27T20:34:21.265161-07:00
model:                 gpt-4-0125-preview
simple_title:         "Satunnaislukujen generointi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen generointi liittyy ennustamattomien sekvenssien tai yksittäisten arvojen tuottamiseen määritellyllä välillä. Ohjelmoijat käyttävät tätä tekniikkaa useista syistä, mukaan lukien simulaatiot, pelit, turvasovellukset ja näytteistysmenetelmät algoritmien testaamiseksi eri olosuhteissa.

## Kuinka:

Javassa satunnaislukujen generointi on mahdollista käyttämällä `java.util`-paketin `Random`-luokkaa tai tiettyihin käyttötarkoituksiin `ThreadLocalRandom`- ja `SecureRandom`-luokkia. Seuraavat esimerkit havainnollistavat näiden luokkien käyttöä.

### Käyttäen `Random`-luokkaa
`Random`-luokka tarjoaa tavan generoida yksinkertaisia pseudo-satunnaislukoja.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Luo Random-olio

        int randInt = rand.nextInt(50); // Generoi satunnaisen kokonaisluvun väliltä 0–49
        double randDouble = rand.nextDouble(); // Generoi satunnaisen liukuluvun välillä 0,0–1,0
        boolean randBoolean = rand.nextBoolean(); // Generoi satunnaisen boolean-arvon
        
        System.out.println("Satunnainen kokonaisluku: " + randInt);
        System.out.println("Satunnainen liukuluku: " + randDouble);
        System.out.println("Satunnainen boolean: " + randBoolean);
    }
}
```

### Käyttäen `ThreadLocalRandom`-luokkaa
Rinnakkaissovelluksissa `ThreadLocalRandom` on tehokkaampi kuin `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // Väliltä 1–100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // Väliltä 1,0–10,0
        
        System.out.println("Satunnainen kokonaisluku: " + randInt);
        System.out.println("Satunnainen liukuluku: " + randDouble);
    }
}
```

### Käyttäen `SecureRandom`-luokkaa
Kryptografisiin operaatioihin `SecureRandom` tarjoaa korkeamman turvallisuustason.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Täyttää tavut turvallisilla satunnaisluvuilla
        
        System.out.println("Turvalliset satunnaiset tavut:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Syväsukellus

Satunnaislukujen generointi on kehittynyt merkittävästi tietokoneiden alkuaikojen jälkeen. Javan `Random`-luokka käyttää lineaarista kongruenssikaavaa pseudo-satunnaislukujen generointiin, jotka ovat deterministisiä eivätkä sovellu korkean turvallisuustason sovelluksiin. Tämä johti `SecureRandom`-luokan käyttöönottoon, joka käyttää monimutkaisempia algoritmeja (esim. SHA1PRNG) tuottaakseen kryptografisesti vahvoja satunnaislukuja.

Siitä huolimatta `Random` ja `SecureRandom` kärsivät puutteista, kuten suorituskyvyn heikkenemisestä monisäikeisissä ympäristöissä. Java 7:ssä esiteltiin `ThreadLocalRandom`-luokka käsittelemään tätä ongelmaa tarjoamalla säietietyt satunnaislukugeneraattorit, jotka merkittävästi parantavat suorituskykyä rinnakkaissovelluksissa.

Vaikka nämä luokat kattavat useimmat tarpeet, äärimmäisen suurille tai erikoistuneille vaatimuksille kehittäjät saattavat tutkia lisäkirjastoja tai kehittää räätälöityjä ratkaisuja. On olennaista valita oikea lähestymistapa käyttötapauksen turvallisuusvaatimusten ja suorituskykyvaatimusten perusteella.
