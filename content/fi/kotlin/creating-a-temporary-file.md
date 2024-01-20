---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tilapäisten tiedostojen luonti on prosessi, jossa luodaan ohjelmistosovellukselle lyhytaikainen tiedosto. Tämän tekeminen auttaa ohjelmoijia tallentamaan väliaikaisia tietoja, jotka auttavat ohjelmaa suorittamaan tehtäviään, mutta eivät ole välttämättömiä pitkän aikavälin tiedon tallennukseen.

## Näin se tehdään:
Alla löydät Kotlin-koodin esimerkin tilapäisen tiedoston luomiseksi sekä tulosteen näytteen:

```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("temporary", ".tmp")

    println("Tiedosto: ${tempFile.absolutePath}")
}
```
Suoritettaessa tämän, saat tulosteen joka näyttää jotain tältä:

```
Tiedosto: /var/folders/xz/fgls0v0s1xd9n57zqlg14d5r0000gn/T/temporary4901238296347224491.tmp
```

## Sukellus syvemmälle:

### Historiallinen tausta:
Tilapäisten tiedostojen käyttö on ollut olemassa niin kauan kuin tiedon tallennus ja hallinta - olennainen osa ohjelmistosuunnittelua.

### Vaihtoehdot:
Vaihtoehtoina väliaikaisten tiedostojen käytölle voidaan pitää muistissa pidettäviä tietorakenteita, kuten hajautustauluja tai luetteloita, mutta nämä voi olla rajoittuneita riippuen järjestelmän muistimäärästä.

### Toteutuksen yksityiskohdat:
`createTempFile` on staattinen metodi, jota kutsutaan `File`-luokalta. Se luo uuden, tyhjän tiedoston määritellylle väliaikaiselle tiedostopolulle ja palauttaa sen viitteenä. Prefix- ja suffixin voi määritellä - ne määrittävät tiedoston nimen aloituksen ja päätyen. Jos suffiksiä ei anneta, ".tmp" käytetään oletuksena.

## Katso myös:
- Java-ajan tilapäisten tiedostojen käytön yksityiskohdat: [Link](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#deleteOnExit())
- Kotlinin tiedostojen käsittelyn oppaat: [Link](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-io.html)
- Pitkäkantoisen tilapäiset tiedostot java.nio.file -luokilla: [Link](https://docs.oracle.com/javase/tutorial/essential/io/file.html#creating)