---
title:    "Java: Tiedoston kirjoittaminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on yleinen tehtävä Java-ohjelmointikielessä. Joskus haluamme tallentaa tietoja tekstidataan, joka voidaan myöhemmin lukea ja käsitellä. Tässä blogipostissa opimme, miten voimme kirjoittaa tekstitiedoston Java-ohjelmoinnissa.

## Kuinka tehdä

Kirjoittaminen tekstitiedostoon Java-ohjelmointikielessä vaatii muutamia vaiheita. Ensinnäkin, meidän täytyy luoda tiedosto, johon haluamme kirjoittaa. Tämä voidaan tehdä ```File``` luokan metodilla ```createNewFile()```. Tämän jälkeen meidän täytyy kytkyä tiedostoon lukijan avulla ja kirjoittaa tietoja sen sisälle. Tämä voidaan tehdä käyttämällä ```BufferedWriter``` luokkaa ja sen metodeita, kuten ```write()``` ja ```newLine()```. Lopuksi emme saa unohtaa sulkea tiedostoa sulkemalla lukija. Alla on esimerkki koodista, joka kirjoittaa tekstitiedoston "tiedostoon.txt" ja lisää siihen kolme riviä tekstiä.

```Java
import java.io.*;

public class KirjoitaTiedostoon {
    public static void main(String[] args) {
        try {
            // Luodaan tiedosto
            File tiedosto = new File("tiedostoon.txt");
            tiedosto.createNewFile();
            
            // Luodaan lukija ja kirjoittaja tiedostoon
            BufferedWriter kirjoittaja = new BufferedWriter(new FileWriter(tiedosto));
            String teksti = "Tämä on ensimmäinen rivi.\n";
            
            // Kirjoitetaan teksti tiedostoon
            kirjoittaja.write(teksti);
            
            // Käytetään uutta riviä ja lisätään teksti
            kirjoittaja.newLine();
            kirjoittaja.write("Tämä on toinen rivi.");
            
            // Käytetään uutta riviä ja lisätään teksti
            kirjoittaja.newLine();
            kirjoittaja.write("Ja tämä on viimeinen rivi.");
            
            // Suljetaan kirjoittaja
            kirjoittaja.close();
            
            System.out.println("Tiedosto on kirjoitettu onnistuneesti.");
        } catch(IOException e) {
            System.out.println("Virhe tiedoston kirjoittamisessa.");
        }
    }
}
```

Yllä olevan koodin suorittamisen jälkeen tiedostoon.txt sisältää seuraavan sisällön:

```
Tämä on ensimmäinen rivi.
Tämä on toinen rivi.
Ja tämä on viimeinen rivi.
```

## Syventyvä tarkastelu

Kun kirjoitamme tekstitiedostoon Java-ohjelmoinnissa, on tärkeää ymmärtää, että käytämme tietyntyyppisiä luokkia ja rajapintoja. Esimerkiksi ```File``` luokka tarjoaa metodeja tiedoston luomiseen ja tarkempaan tietoon sen ominaisuuksista, kuten nimi ja polku. ```BufferedWriter``` luokka tarjoaa tehokkaan tavan kirjoittaa suuria määriä tietoa tiedostoon. On myös tärkeää huomata, että Java tarjoaa myös muita vaihtoehtoja tekstitiedoston kirjoittamiseen, kuten ```PrintWriter``` luokka.

## Katso myös

- [Java File API](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java BufferedWritter API](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)
- [Java PrintWriter API](https://docs.oracle.com/javase/8/docs/api/java/io/PrintWriter.html)