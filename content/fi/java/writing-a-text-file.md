---
title:    "Java: Tekstitiedoston kirjoittaminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on olennainen osa ohjelmointia, ja teksititiedoston kirjoittaminen on hämmästyttävä tapa tallentaa tietoja ja lukea niitä myöhemmin. Se on myös tärkeä osa tiedonsiirtoa ja tiedostojen hallintaa.

## Miten tehdä

Java tarjoaa helpon tavan kirjoittaa tekstitiedostoja. Voit käyttää FileWriter- ja BufferedWriter-luokkia kirjoittamalla tekstitiedostoon.

Seuraava koodiesimerkki näyttää, kuinka luodaan uusi tekstitiedosto, kirjoitetaan siihen rivi kerrallaan ja suljetaan tiedosto lopuksi:

```Java

import java.io.*;

public class TextFileWriter {

    public static void main (String[] args) {

        try {

            // Luo uusi tekstitiedosto nimellä "teksti.txt"
            FileWriter fw = new FileWriter("teksti.txt");

            // Luo uusi BufferedWriter kirjoittamaan tekstitiedostoon
            BufferedWriter bw = new BufferedWriter(fw);

            // Kirjoita "Tämä on ensimmäinen rivi" teksti.txt-tiedostoon
            bw.write("Tämä on ensimmäinen rivi");

            // Kirjoita "Tämä on toinen rivi" teksti.txt-tiedostoon
            bw.write("Tämä on toinen rivi");

            // Sulje BufferedWriter
            bw.close();

            System.out.println("Tekstitiedosto luotu ja siihen kirjoitettu.");

        } catch (IOException e) {

            System.out.println("Virhe tiedostoa kirjoitettaessa.");

            e.printStackTrace();
        }
    }
}
```

Seuraava koodiesimerkki näyttää, kuinka lukea luotu teksti.txt-tiedosto ja tulostaa sen sisältö konsoliin:

```Java
import java.io.*;

public class TextFileReader {

    public static void main (String[] args) {

        try {

            // Luo uusi FileReader ja anna sille luotu tekstitiedosto
            FileReader fr = new FileReader("teksti.txt");

            // Luo uusi BufferedReader lukeaksesi FileReaderin
            BufferedReader br = new BufferedReader(fr);

            // Luo muuttuja merkkijonolle, johon tallennetaan luetut rivit
            String line = "";

            // Käy läpi tiedoston rivit ja tulosta ne konsoliin
            while ((line = br.readLine()) != null) {

                System.out.println(line);
            }

            // Sulje BufferedReader
            br.close();

        } catch (IOException e) {

            System.out.println("Virhe tiedostoa luettaessa.");

            e.printStackTrace();
        }
    }
}
```

Kun suoritat nämä kaksi esimerkkiä, saat seuraavan tulosteen konsoliin:

```
Tämä on ensimmäinen rivi
Tämä on toinen rivi
```

On huomattava, että jos teksti.txt-tiedosto jo on olemassa, uudet kirjoitetut rivit korvaavat aiemmat rivit. Tämä voidaan välttää käyttämällä FileWriterin konstruktoria, jossa on parametrina boolean-tieto true. Tämä merkitsee append-tilaa ja uudet rivit lisätään tiedoston loppuun eikä korvata niitä.

```Java
FileWriter fw = new FileWriter("teksti.txt", true);
```

## Syvällinen sukellus

Tiedostojen kirjoittaminen Java-kielellä on mahdollista muutaman eri luokan avulla. FileWriter ja BufferedWriter ovat yleisimmin käytetyt luokat, mutta myös muita luokkia, kuten PrintWriter, voidaan käyttää tiedostojen kirjoittamiseen. Näiden luokkien käyttö on mahdollista myös tiedoston luomisen yhteydessä käyttämällä File-luokkaa.

Tiedostoon kirjoittamisen lisäksi Java-kielellä on myös mahdollista lukea tiedostoja eri tavoilla, esimerkiksi File-luokan avulla tai Scanner-luokalla.

## Katso