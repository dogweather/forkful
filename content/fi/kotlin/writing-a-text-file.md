---
title:                "Kotlin: Tiedostotiedoston kirjoittaminen"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on olennainen osa ohjelmistokehitystä. Se mahdollistaa tiedon tallentamisen ja lukemisen ohjelman sisältä ja tämä voi olla hyödyllistä monissa sovelluksissa.

## Kuinka tehdä

Kotlinilla on monia tapoja kirjoittaa tekstitiedostoja, mutta yksi yksinkertainen tapa on käyttää `FileWriter` -luokkaa. Tässä esimerkissä kirjoitamme yksinkertaisen tekstin `"Hei maailma!"` tiedostoon nimeltä `"teksti.txt"`:

```Kotlin
import java.io.FileWriter

fun main() {
  val teksti = "Hei maailma!"
  
  val tiedosto = FileWriter("teksti.txt")
  // Kirjoitetaan tiedostoon
  tiedosto.write(teksti)
  // Suljetaan tiedosto
  tiedosto.close()
}
```

Tämän jälkeen tiedostomme sisältää tekstin "Hei maailma!". Voit avata sen tekstieditorilla ja tarkistaa sen sisällön.

On kuitenkin tärkeää huomata, että tiedoston kirjoitus voi aiheuttaa virheitä, joten on tärkeää käsitellä mahdolliset poikkeukset. Aina on myös hyvä sulkea tiedosto `close()`-metodilla varmistaaksesi, että kaikki muutokset tallentuvat.

## Syvällinen sukellus

Tiedoston kirjoittaminen voi olla monimutkaisempi prosessi, riippuen siitä millaista tietoa haluat tallentaa. Voit käyttää `BufferedWriter`-luokkaa kirjoittaaksesi suurempia määriä dataa tai `PrintWriter`-luokkaa kirjoittaaksesi muotoiltua tekstiä.

Voit myös käyttää `FileWriter`-luokan erilaisia konstruktoreita määrittääksesi haluatko kirjoittaa tiedoston loppuun (`FileWriter(String, true)`) vai aloittaa uuden tiedoston (`FileWriter(String, false)`).

Kaiken kaikkiaan on tärkeää tutustua erilaisiin vaihtoehtoihin ja valita ne jotka parhaiten sopivat tarpeisiisi.

## Katso myös

- [Tiedostojen lukeminen ja kirjoittaminen Kotlinilla](https://kotlinlang.org/docs/tutorials/kotlin-for-py/reading-writing-files.html)
- [Kotlinin java.io-paketin dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file-writer/index.html)