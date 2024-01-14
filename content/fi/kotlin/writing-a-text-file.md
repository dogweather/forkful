---
title:    "Kotlin: Tekstitiedoston kirjoittaminen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on tärkeä osa ohjelmoinnin prosessia. Kun luomme ohjelmia, tarvitsemme usein tallennustilaa tietojen tallentamiseen. Tässä tapauksessa tekstitiedostot ovat hyvin kätevä työkalu, sillä ne mahdollistavat tiedon tallentamisen helposti ja yksinkertaisesti.

## Miten

Kotlinilla on helppo kirjoittaa tiedostoon, ja se vaatii vain muutaman yksinkertaisen askeleen. Ensinnäkin, sinun täytyy luoda uusi tekstitiedosto nimipääteellä `.txt`. Tämän jälkeen voit käyttää `FileWriter`-luokkaa avaamaan ja kirjoittamaan tiedostoon.

```Kotlin
import java.io.FileWriter

fun main() {
    val tiedosto = FileWriter("uusi_tiedosto.txt")
    tiedosto.write("Tämä on ensimmäinen rivi.")
    tiedosto.append("\nTämä on toinen rivi.")
    tiedosto.close()
}
```

Käyttämämme `FileWriter`-luokka antaa meille mahdollisuuden avata ja kirjoittaa tiedostoon. Ensimmäisellä kerralla kirjoitamme tekstiä `write()`-metodilla ja toisella kerralla lisäämme tekstiä olemassa olevaan tiedostoon `append()`-metodilla. Lopuksi `.close()` sulkee tiedoston ja tallentaa kaikki tekstit.

## Syvemmälle

Kirjoittaessa on tärkeää myös ottaa huomioon, miten kirjoitettu tiedosto tallennetaan. Käyttämämme `FileWriter`-luokka ei aina tallenna tiedostoa samaan paikkaan, josta sen avasimme. Tiedosto tallennetaan aina nykyisen projektimme juurikansioon.

Voit myös käyttää `BufferedWriter`-luokkaa, joka antaa meille mahdollisuuden kirjoittaa enemmän tekstiä kerralla. Tämä saattaa nopeuttaa kirjoitusprosessia, koska jokaisella `write()`-kutsulla ei tallenneta tiedostoon heti, vaan vasta `flush()`-metodia kutsuttaessa.

```Kotlin
import java.io.BufferedWriter
import java.io.FileWriter

fun main() {
    val tiedosto = FileWriter("uusi_tiedosto.txt")
    val bufferedWriter = BufferedWriter(tiedosto)
    bufferedWriter.write("Tämä on ensimmäinen rivi.")
    bufferedWriter.newLine()
    bufferedWriter.write("Tämä on toinen rivi.")
    
    bufferedWriter.close()
}
```

Tiedostoon kirjoitettavan tekstin lisäksi voit myös käyttää `PrintWriter`-luokkaa tulostamaan muotoiltua tekstiä.

```Kotlin
import java.io.FileWriter
import java.io.PrintWriter

fun main() {
    val tiedosto = FileWriter("uusi_tiedosto.txt")
    val printWriter = PrintWriter(tiedosto)
    
    printWriter.printf("Käyttäjänimi %s kirjoittaa tiedostoon.", "Esimerkki")
    printWriter.close()
}
```

Kotlinin virallinen dokumentaatio tarjoaa lisää tietoa tiedostonkirjoituksesta ja sen eri vaihtoehdoista. Voit myös tutustua Javan `FileWriter`-luokkaan, johon Kotlinin versio perustuu.

## Katso myös

- [Kotlinin käsikirja tiedostojen lukemiseen ja kirjoittamiseen](https://kotlinlang.org/docs/tutorials/kotlin-for-py/read-write-files.html)
- [Java FileWriter-luokka](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)