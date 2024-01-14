---
title:    "Java: Väliaikaisen tiedoston luominen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit luoda väliaikaisen tiedoston Java-ohjelmoinnissa? On useita syitä, miksi voit tarvita väliaikaisia tiedostoja ohjelmoidessasi. Esimerkiksi voit haluta tallentaa käyttäjän lähettämiä tiedostoja väliaikaisesti, ennen kuin käsittelet niitä pysyvällä tavalla. Tai ehkä ohjelmasi tarvitsee väliaikaisen tiedoston laskentatarkoituksiin. Riippumatta syystä, väliaikaiset tiedostot ovat erittäin hyödyllisiä Java-ohjelmoinnissa.

## Kuinka

Tässä esimerkissä näytämme, kuinka luoda väliaikainen tiedosto Java-ohjelmassa ja tulostaa sen polun konsoliin.

```Java
import java.io.File; // Tuodaan File-luokka

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Luodaan väliaikainen tiedosto annetussa kansiossa ja nimellä
            File tempFile = File.createTempFile("example", ".txt", new File("temp_files"));
            
            // Tulostetaan tiedoston polku konsoliin
            System.out.println("Väliaikaisen tiedoston polku: " + tempFile.getAbsolutePath());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Tämä koodi tuottaa seuraavan tulosteen:

```
Väliaikaisen tiedoston polku: C:\temp_files\example8282462708444945955.txt
```

Kuten näet, väliaikaisen tiedoston polku on luotu automaattisesti ja se sijaitsee annetussa kansiossa. Voit myös luoda väliaikaisen tiedoston ilman antamalla kansio- tai tiedostonimeä, jolloin käyttöjärjestelmä määrittää tiedoston nimen ja sijainnin.

## Syvemmälle

Java tarjoaa useita erilaisia tapoja luoda väliaikaisia tiedostoja, kuten ```File.createTempFile```, ```Files.createTempFile``` ja ```Files.createTempDirectory```. On tärkeää muistaa, että väliaikainen tiedosto on tarkoitettu tilapäiseen käyttöön ja sen tulee poistua ohjelman suorituksen jälkeen. Voit käyttää ```deleteOnExit()``` -metodia varmistaaksesi, että väliaikainen tiedosto poistetaan ohjelman loputtua.

Lisäksi voit määrittää väliaikaisen tiedoston attribuuttien, kuten tiedoston nimen, sijainnin ja oikeuksien hallintaa. Voit myös käyttää ```Path``` ja ```Files``` luokkia helpottamaan tiedostojen käsittelyä.

## Katso myös

- [Oracle Java dokumentaatio väliaikaisista tiedostoista](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Väliaikaisen tiedoston luominen JavaFX-ohjelmassa](https://www.geeksforgeeks.org/javafx-file-class-createtempfile-method-with-examples/)
- [HDBuffer - JavaKoulu opetusmateriaali väliaikaisista tiedostoista](https://hd-buffer.blogspot.com/2019/08/kayttotarkoitus-valiaikaiset-tiedostot-java.html)