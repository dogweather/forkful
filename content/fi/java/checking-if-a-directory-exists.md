---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- fi/java/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:10.923123-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Tarkastus siitä, onko kansio olemassa Javassa, on perustehtävä, joka sisältää tiedostojärjestelmän kansion olemassaolon varmistamisen ennen siitä lukemista, sinne kirjoittamista tai mitä tahansa toimintoja, jotka edellyttävät sen olemassaoloa. Tämä on ratkaisevaa virheiden tai poikkeusten välttämiseksi ohjelmissa, jotka toimivat tiedostojärjestelmän kanssa, varmistaen sujuvamman suorituksen ja paremman käyttäjäkokemuksen.

## Kuinka:
Javassa on useita tapoja tarkistaa, onko kansio olemassa, ensisijaisesti käyttämällä `java.nio.file.Files`- ja `java.io.File`-luokkia.

**Käyttäen `java.nio.file.Files`**:

Tämä on suositeltu menetelmä uusimmissa Java-versioissa.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Määritä kansion polku tässä
        String directoryPath = "polku/kansioon";

        // Tarkistetaan, onko kansio olemassa
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("Kansio on olemassa.");
        } else {
            System.out.println("Kansiota ei ole olemassa.");
        }
    }
}
```
**Esimerkkituloste**:
```
Kansio on olemassa.
```
Tai
```
Kansiota ei ole olemassa.
```

**Käyttäen `java.io.File`**:

Vaikkakin `java.nio.file.Files` on suositeltu, myös vanhempaa `java.io.File`-luokkaa voidaan käyttää.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Määritä kansion polku tässä
        String directoryPath = "polku/kansioon";

        // Luodaan File-objekti
        File directory = new File(directoryPath);

        // Tarkistetaan, onko kansio olemassa
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("Kansio on olemassa.");
        } else {
            System.out.println("Kansiota ei ole olemassa.");
        }
    }
}
```
**Esimerkkituloste**:
```
Kansio on olemassa.
```
Tai
```
Kansiota ei ole olemassa.
```

**Käyttäen kolmannen osapuolen kirjastoja**:

Vaikka standardi Java-kirjasto yleensä riittää tähän tehtävään, kolmannen osapuolen kirjastot kuten Apache Commons IO tarjoavat lisää tiedostonkäsittelyyn liittyviä apuvälineitä, jotka saattavat olla hyödyllisiä monimutkaisemmissa sovelluksissa.

**Apache Commons IO**:

Lisää ensin Apache Commons IO riippuvuus projektiisi. Sen jälkeen voit käyttää sen ominaisuuksia tarkistaaksesi kansion olemassaolon.

```java
// Oletetaan, että Apache Commons IO on lisätty projektiin

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Määritä kansion polku tässä
        String directoryPath = "polku/kansioon";

        // Käyttäen FileUtilsia tarkistamaan
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("Kansio on olemassa.");
        } else {
            System.out.println("Kansiota ei ole olemassa.");
        }
    }
}
```

**Huom**: `FileUtils.directoryContains` tarkistaa, sisältääkö kansio tietyn tiedoston, mutta toisena argumenttina `null` käyttämällä, voit käyttää sitä tarkistamaan kansion olemassaolon. Ole varovainen, sillä tämä ei välttämättä ole menetelmän suoraviivaisin tai tarkoitettu käyttötapa.

**Esimerkkituloste**:
```
Kansio on olemassa.
```
Tai
```
Kansiota ei ole olemassa.
```
