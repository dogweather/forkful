---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Kotlin: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Checking if a directory exists tarkoittaa tietokoneen ohjelmassa tarkistamista, onko tiedostojärjestelmässä hakemistoa kyseisellä nimellä. Ohjelmoijat tekevät tätä voidakseen tehdä tiettyjä toimintoja, kuten avata tai luoda uusia tiedostoja.

## Kuinka tehdä:
Esimerkkejä koodista ja tulosteista käyttäen ```Kotlin ... ``` -koodilohkoja.

```Kotlin
val directory = File("C:\\Users\\Downloads")
if (directory.exists()) {
    println("Hakemisto on olemassa.")
} else {
    println("Hakemistoa ei löytynyt.")
}
```
Tällä koodilla tarkistetaan, onko `Downloads` hakemistoa olemassa. Jos hakemisto löytyy, tulostetaan "Hakemisto on olemassa". Muussa tapauksessa tulostetaan "Hakemistoa ei löytynyt".

```Kotlin
val directory = File("C:\\Users\\Documents\\NewFolder")
if (!directory.exists()) {
    directory.mkdir()
    println("Uusi hakemisto luotiin.")
} else {
    println("Hakemisto on jo olemassa.")
}
```
Tässä esimerkissä tarkistetaan, onko `NewFolder` hakemistoa jo olemassa. Jos hakemistoa ei ole, se luodaan ja tulostetaan "Uusi hakemisto luotiin". Jos hakemisto on jo olemassa, tulostetaan "Hakemisto on jo olemassa".

## Syvällinen katsaus:
Historiallista taustaa, vaihtoehtoja ja toteutusyksityiskohtia hakemistojen olemassaolon tarkistamiselle.

Tämän toiminnon toteuttamiseksi voidaan käyttää myös `Java.io.File` -luokan metodia `exists()`. Tämä metodi palauttaa boolean-arvon sen mukaan, löytyykö annetulla polulla olevaa hakemistoa vai ei.

Toinen tapa tarkistaa hakemiston olemassaolo on käyttää `java.nio.file.Files` -luokan metodia `exists()`. Tämä metodi myös palauttaa boolean-arvon ja sitä voidaan käyttää tarkistamaan tiedostojärjestelmän objektin olemassaoloa.

## Katso myös:
Tässä on joitain lähteitä ja ohjeita, jotka voivat auttaa sinua tarkistamaan hakemistojen olemassaolon:
- [Java-dokumentaatio hakemiston olemassaolon tarkastamisesta](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
- [Kotlin-dokumentaatio tiedostojen ja hakemistojen käsittelyyn](https://kotlinlang.org/docs/tutorials/native/java-interop-file-system.html)
- [Stack Overflow -kysymys hakemiston olemassaolon tarkastamisesta](https://stackoverflow.com/questions/27074401/how-to-check-if-a-directory-exists-in-kotlin)