---
title:    "Kotlin: Tiedostotiedoston lukeminen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Miksi lukea tiedostoa Kotlin-ohjelmoinnilla?

Tiedostojen lukeminen on olennainen osa ohjelmointia, ja teksti- tai datatiedostojen lukeminen on erityisen tärkeää. Tämä voi sisältää esimerkiksi käyttäjän syöttämiä tietoja tai muuta ulkoista tietoa, jota haluat käsitellä ohjelmassasi. Kotlinin avulla tiedostojen lukeminen on helppoa ja tehokasta.

## Miten tehdä se?

Kotlinissa voit lukea tiedoston käyttämällä `File`-luokkaa ja sen `reader()`-metodia. Voit myös käyttää `useLines`-metodia, joka lukee tiedoston sisällön rivi kerrallaan ja palauttaa sen `Sequence`-oliona. Esimerkiksi:

```Kotlin
val tiedosto = File("tiedostonimi.txt")
var rivi = ""
tiedosto.reader().forEachLine {
    rivi += "$it\n"
}
println(rivi)
```
Tämä koodi lukee tiedoston ja tallentaa sen sisällön muuttujaan `rivi`. Voit myös käyttää `useLines`-metodia, joka olisi yhtä tehokas tapa lukea ja käsitellä tiedosto. Käytä `try` ja `catch` -lohkoja käsittelemään mahdolliset poikkeukset.

## Syvällisempi tarkastelu

Kotlin tarjoaa kätevän tavan käsitellä tiedostoja käyttämällä `File`-luokkaa ja sen metodeja. Tämä luokka tarjoaa myös muita hyödyllisiä metodeja, kuten `writeText()` ja `appendText()`, joilla voit tallentaa tai lisätä sisältöä tiedostoon.

Tiedoston luku voi olla myös haastavaa erityisesti, kun kyseessä on suuri tiedosto. Kotlin tarjoaa `BufferedReader`-luokan, joka voi parantaa suorituskykyä, kun sitä käytetään yhdessä `File`-luokan kanssa. Se lukee tiedoston näytön yli ja luo välimuistiin pakattuja rivejä, mikä nopeuttaa tiedoston lukemista.

## Katso myös

- JetBrainsin virallinen dokumentaatio tiedostojen käsittelystä Kotlinissa: https://kotlinlang.org/docs/tutorials/kotlin-for-py/files.html
- Tiedostojen lukeminen ja kirjoittaminen Kotlinilla: https://www.programiz.com/kotlin-programming/file-input-output
- Vertailu StreamReaderin ja BufferedReaderin välillä: https://stackoverflow.com/questions/19648507/using-bufferedreader-or-scanner-to-read-a-text-file-in-java 

# Katso myös

- Kotlinin virallinen verkkosivusto: https://kotlinlang.org/
- Suomenkielinen Kotlin-opetusohjelma: https://javajokaikille.fi/kotlin-opetuskurssi/