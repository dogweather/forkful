---
title:    "Kotlin: Tarkistetaan löytyykö hakemistoa"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Monet Kotlin-ohjelmoijat saattavat joutua tarkistamaan, onko hakemisto olemassa, ennen kuin he voivat suorittaa tiettyjä toimintoja. Tässä blogikirjoituksessa tarkastelemme, miksi tämä on tärkeää ja miten se voidaan tehdä helposti Kotlinilla.

## Miten

Tarkistamalla hakemiston olemassaoloa voi olla useita syitä, kuten tiettyjen tiedostojen läsnäolon varmistaminen ennen niiden käsittelyä tai tietyn toiminnallisuuden toteuttaminen vain, jos hakemisto löytyy. Alla on esimerkki siitä, miten voit tarkistaa, onko hakemisto olemassa Kotlinilla:

```Kotlin
val directory = File("polku/hakemistoon")
if (directory.exists()) {
    println("Hakemisto löytyi!")
} else {
    println("Hakemistoa ei löytynyt.")
}
```

Tämä koodiesimerkki luo uuden File-olion ja käyttää sille `exists()`-metodia, joka palauttaa boolean-arvon riippuen siitä, löytyykö kyseinen hakemisto vai ei. Tämän jälkeen tulostetaan sen perusteella haluttu viesti. Voit myös halutessasi lisätä tarkistuksia, kuten `isDirectory()`-metodin avulla, jotta varmistat, että kyseessä on nimenomaan hakemisto eikä esimerkiksi tiedosto.

## Syvempi sukellus

Kotlinilla on myös mahdollista luoda uusi hakemisto, jos sitä ei ole olemassa. Tämä voidaan tehdä `mkdir()`-metodilla, joka luo uuden hakemiston annetun polun perusteella. Alla on esimerkki:

```Kotlin
val directory = File("polku/uuteen/hakemistoon")
if (directory.mkdir()) {
    println("Uusi hakemisto luotiin!")
} else {
    println("Tapahtui virhe uuden hakemiston luomisessa.")
}
```

Tämä koodi tarkistaa ensin, onko kyseistä hakemistoa jo olemassa. Jos ei, se luo uuden `File`-olion ja käyttää `mkdir()`-metodia luodakseen uuden hakemiston. Tämän jälkeen tulostetaan haluttu viesti sen perusteella, onnistuiko hakemiston luominen vai ei.

## Katso myös

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/reference/io.html)
- [Tietoa hakemistojen ja tiedostojen käsittelystä Kotlinilla](https://www.baeldung.com/kotlin-directory-files)
- [Esimerkkejä hakemistojen luomisesta ja tarkistamisesta Kotlinilla](https://www.programiz.com/kotlin-programming/directory)