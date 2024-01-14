---
title:    "Kotlin: Tarkista, onko hakemisto olemassa"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi
On monia syitä miksi voit tarvita tarkistaa, onko hakemisto olemassa. Esimerkiksi voit haluta varmistaa, että hakemisto on olemassa ennen kuin tallennat tiedostoja siihen tai käytät sitä osana tiedostojen polkua.

## Näin teet sen
Tarkistaminen, onko hakemisto olemassa, on melko yksinkertaista käyttäen Kotlinin File-luokkaa. Seuraavassa on esimerkki, jossa tarkistetaan, onko "hakemisto" niminen hakemisto olemassa ja tulostetaan vastaava tieto:

```Kotlin
val directory = File("hakemisto")
if (directory.exists()){
    println("Hakemisto on olemassa.")
} else {
    println("Hakemistoa ei ole olemassa.")
}
```

Tässä tapauksessa käytetään "exists()" funktiota, joka palauttaa totuusarvon sen perusteella, onko hakemisto olemassa vai ei.

## Syvempi sukellus
Tarkistaaksesi, onko hakemisto olemassa, ohjelma lukee aluksi hakemisto rakenteen ja varmistaa, että hakemisto on olemassa ja käytettävissä. Tämän jälkeen se tarkistaa, onko hakemiston merkintä oikein ja sallii yhteyden hakemistoon.

Jos hakemisto ei ole käytettävissä, ohjelma heittää "SecurityException" poikkeuksen, joka osoittaa, että käyttäjällä ei ole oikeuksia käyttää kyseistä hakemistoa. Tämä on tärkeää pitää mielessä, jos käytät tarkistusta hakemistoon, johon sinulla ei ole tarvittavia oikeuksia.

## Katso myös
- [Kotlinin File-luokan dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Tiedostojärjestelmien perusteet Kotlinilla](https://www.raywenderlich.com/155083/android-tutorial-file-io-kotlin)
- [Kotlin for Android: File I/O and Permissions](https://www.raywenderlich.com/4330491-kotlin-for-android-file-io-and-permissions)