---
title:                "Tiedoston lukeminen"
html_title:           "Kotlin: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Ennen kuin aloitamme syvemmin puhumaan tekstitiedostojen lukemisesta Kotlinilla, ensin katsotaan, miksi tämä taito olisi hyödyllinen. Tekstitiedostot ovat tärkeitä ja yleisesti käytössä olevia tiedostoja, jotka sisältävät tekstimuotoista dataa. Niitä käytetään usein tiedon tallentamiseen ja vaihtamiseen, ja siksi on tärkeää osata lukea niitä ohjelmoinnin avulla.

## Miten

Tekstitiedostojen lukeminen Kotlinilla on helppoa ja yksinkertaista muutamalla yksinkertaisella askeleella. Ensimmäiseksi, sinun täytyy luoda `File`-olio, joka viittaa haluamaasi tekstitiedostoon. Esimerkiksi `val file = File("tietoja.txt")`. Sitten voit käyttää `BufferedReader`-luokkaa luomaan lukijan tiedostollesi ja käyttämään `forEachLine`-funktiota lukeaksesi tiedoston sisällön rivi riviltä.

```Kotlin
val file = File("tietoja.txt")
val reader = BufferedReader(file.reader())

reader.forEachLine {
    println(it) // tulostetaan jokainen rivi
}
```

Tämä koodi lukee tekstitiedoston `tietoja.txt` sisällön ja tulostaa sen konsoliin.

## Syvempi sukellus

Tekstitiedostojen lukeminen Kotlinilla ei rajoitu vain yksittäisten rivien lukemiseen, vaan voit myös käsitellä tiedoston sisältöä haluamallasi tavalla. Esimerkiksi voit käyttää `readLines`-funktiota, joka palauttaa listan kaikista tiedoston riveistä. Tämän jälkeen voit käyttää listan erilaisia metodeja, kuten `filter` tai `map`, muokatakseen ja työstääkseen tiedoston sisältöä.

```Kotlin
val file = File("tietoja.txt")
val lines = file.readLines()

// Suodata ja tulosta vain rivit, jotka sisältävät halutun merkkijonon
lines.filter{ it.contains("Kotlin") }.forEach {
    println(it)
} 
```

Voit myös käyttää `useLines`-funktiota, joka lukee rivin kerrallaan tiedostoa, mikä on tehokkaampi tapa käsitellä suuria tiedostoja.

```Kotlin
val file = File("tietoja.txt")

// Alustetaan muuttuja, johon tallennetaan haluttu data
var total = 0

file.useLines { lines ->
    total = lines.map{ it.toInt() }.sum() // muutetaan rivit Int-tyypeiksi ja lasketaan niiden summa
}

println("Tiedostossa on yhteensä $total numeroa.") // tulostaa esimerkiksi "Tiedostossa on yhteensä 30 numeroa."
```

## Katso myös

* [Kotlinin virallinen sivusto](https://kotlinlang.org/)
* [Kotlinin dokumentaatio](https://kotlinlang.org/docs/home.html)
* [Tekstitiedostojen käsittely Kotlinilla](https://kotlinexpertise.com/reading-files-with-kotlin/)