---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:10.923123-07:00
description: "Kuinka: Javassa on useita tapoja tarkistaa, onko kansio olemassa, ensisijaisesti\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `java.nio.file.Files`- ja `java.io.File`-luokkia. **K\xE4\
  ytt\xE4en\u2026"
lastmod: '2024-03-13T22:44:56.459599-06:00'
model: gpt-4-0125-preview
summary: "Javassa on useita tapoja tarkistaa, onko kansio olemassa, ensisijaisesti\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `java.nio.file.Files`- ja `java.io.File`-luokkia."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

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
