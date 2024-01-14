---
title:                "Kotlin: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat varmistaa, että kansio on olemassa ennen kuin aloitat tiedostojen hallinnan. Ensinnäkin, se auttaa estämään ohjelmistovirheitä ja kaatumisia, koska voit olla varma, että tarvittavat tiedostot ovat saatavilla. Lisäksi, jos haluat lukea tiedostoja tai tallentaa tiedostoja tiettyyn kansioon, on tärkeää varmistaa, että kyseinen kansio on olemassa.

## Miten

Varmistaaksesi, että kansio on olemassa, voit käyttää Kotlinin "exists()" -toimintoa. Alla olevassa esimerkissä tarkistamme, onko kansio nimeltä "kuvat" olemassa tiedostojärjestelmässä.

```Kotlin
val kansio = File("kuvat")
if (kansio.exists()) {
    println("Kansio on olemassa!")
} else {
    println("Kansiota ei löytynyt.")
}
```

### Tuloste:
```
Kansio on olemassa!
```

## Syväsukellus

"Exists()" -toiminto tarkistaa, onko tiedostopolulla sijaitseva kansio tai tiedosto olemassa. Se palauttaa boolean-arvon, joka on "true", jos kansio on olemassa ja "false", jos sitä ei ole. Jos et ole varma, onko kansio olemassa vai et, voit myös käyttää "createNewFile()" -toimintoa. Tämä luo uuden tiedoston, jos kansio tai tiedosto ei ole olemassa ja palauttaa "false", jos se on jo olemassa.

Voit myös käyttää "mkdir()" -toimintoa luodaksesi kansio, jos sitä ei ole olemassa. Tämä on hyödyllistä, jos haluat varmistaa, että kansio on käytettävissä ennen tiedostojen luomista tai käsittelyä.

## Katso myös

- [Kotlin - Tiedostojen hallinta](https://kotlinlang.org/docs/tutorials/native/files.html)
- [Kotlin - Tiedostojen luominen ja käsittely](https://kotlinlang.org/docs/tutorials/kotlin-for-server-side/file-io.html) 
- [Java - Tarkista hakemisto Java File IO -luokalla](https://www.tutorialspoint.com/java/java_using_filewriter.htm)