---
title:                "Tilapäistiedoston luominen"
html_title:           "Java: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi

Luodessasi ohjelmia Java-ohjelmointikielellä, saatat joutua työskentelemään väliaikaisten tiedostojen kanssa. Näiden väliaikaisten tiedostojen luominen voi olla hyödyllistä, esimerkiksi kun haluat tallentaa väliaikaisia tietoja järjestelmän suorituksen aikana. Tässä artikkelissa kerromme, miksi ja miten luodaan väliaikainen tiedosto käyttäen Javaa.

# Miten

Java tarjoaa sisäänrakennetun TempFile-luokan, joka mahdollistaa väliaikaisen tiedoston luomisen helposti. Seuraavassa esimerkissä näet, miten se toimii:

```Java
import java.io.File;
import java.io.IOException;

public class TempFileExample {

    public static void main(String[] args) throws IOException {

        File myTempFile = File.createTempFile("myTempFile", ".txt"); // Luodaan väliaikainen tiedosto nimeltä "myTempFile.txt"
        System.out.println("Luotiin väliaikainen tiedosto: " + myTempFile.getName());
        myTempFile.deleteOnExit(); // Tiedosto poistetaan automaattisesti, kun ohjelma suljetaan
    }
}
```

Tulostus näyttää seuraavalta:

```
Luotiin väliaikainen tiedosto: myTempFile6427500394267177990.txt
```

Kuten näet, väliaikainen tiedosto nimetään automaattisesti ja se poistetaan automaattisesti ohjelman sulkiessa.

# Syventävä tarkastelu

TempFile-luokka perustuu Javan sisäänrakennettuun File-luokkaan, joten sillä on samanlaiset ominaisuudet ja toiminnot. Voit myös määrittää haluamasi hakemiston, jonne väliaikainen tiedosto luodaan. Lisäksi voit määrittää, haluatko tiedoston poistuvan automaattisesti ohjelman sulkiessa tai haluatko poistaa sen manuaalisesti.

# Katso myös

- [Java TempFile-luokan virallinen dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Java File-luokan virallinen dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java-ohjelmoinnin opiskelu aloittelijoille](https://java.com/fi/download/help/whatis_java.html)