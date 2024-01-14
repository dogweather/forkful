---
title:                "Kotlin: Tekstitiedoston lukeminen"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tiedostojen käsittely on olennainen osa ohjelmointia, ja usein joudumme käsittelemään tekstiä tiedoston muodossa. Tästä syystä on tärkeää ymmärtää, miten luemme tekstitiedostoja Kotlin-ohjelmoinnissa.

## Kuinka

```Kotlin
fun lueTiedosto(polku: String): List<String> {
    File(polku).useLines { lines -> return lines.toList() }
}

val tiedot = lueTiedosto("tiedosto.txt")
for (tieto in tiedot) {
    println(tieto)
}
```

Esimerkissä luomme funktion "lueTiedosto", joka hyödyntää "File"-luokkaa ja sen "useLines"-metodia avatakseen ja lukeakseen tekstitiedoston. Tämän jälkeen voimme käyttää funktiota lähettämällä sille halutun tiedostopolun parametrina. Tuloksena saamme listan, jossa jokainen rivivastaa yhtä tiedoston riviä. Lopuksi käytämme "for"-silmukkaa tulostamaan jokainen rivi konsoliin.

## Syvällinen sukellus

Luettuamme tekstitiedoston, haluamme mahdollisesti käsitellä sen sisältöä tarkemmin. Voimme käyttää esimerkiksi String-luokan "split"-metodia jakamaan tiedoston rivit erillisiksi osiksi.

```Kotlin
val tiedot = lueTiedosto("tiedosto.txt")
for (tieto in tiedot) {
    val osat = tieto.split(",")
    println("${osat[0]} on ${osat[1]} vuoden ikäinen.")
}
```

Tässä esimerkissä hyödynnetään ","-merkkiä erotinmerkkinä ja tulostetaan jokaiselle riville tietynlainen lause.

## Katso myös

- [Kotlinin virallinen dokumentaatio tiedostojen käsittelystä](https://kotlinlang.org/docs/tutorials/kotlin-for-py/how-to-read-write-files.html)
- [Esimerkkejä tekstitiedoston lukemisesta Kotlinilla](https://www.educative.io/edpresso/how-to-read-a-file-using-kotlin)
- [Interaktiivisia harjoituksia tiedostonkäsittelyyn liittyen](https://www.codingame.com/ide/puzzle/file-on-the-road-big-data)