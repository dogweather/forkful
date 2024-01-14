---
title:                "Kotlin: Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Tekstin muuttaminen pienaakkosiksi on tärkeää, kun haluat verrata tai etsiä tietoa tekstissä. Muunnaessa merkkijonon pienaakkosiksi, voit varmistaa että kaikki kirjaimet ovat samassa muodossa.

## Miten

```Kotlin
fun main() {
    val merkkijono = "TÄMÄ ON ESIMERKKI"
    println(merkkijono.toLowerCase())
}
```
**Output:** *tämä on esimerkki*

Muuttamista varten käytämme `toLowerCase()` -metodia, jonka avulla voimme muuttaa merkkijonon pienaakkosiksi. Tämä metodi auttaa myös sivuuttamaan kirjainten väliset erot, kuten ä tai ö.

Voimme myös käyttää `toLowerCase(Locale.ENGLISH)` tai muuta tiettyä kieltä vastaavaa `Locale` -arvoa, jotta merkkijono muutetaan kyseiselle kielelle tyypilliseen muotoon.

## Syvällinen sukellus

Kotlinissa `String` -luokassa on `toLowerCase()` -metodi, joka palauttaa merkkijonosta uuden olion, joka sisältää kaikki merkit pienaakkosina. Tämä metodi käyttää oletusarvoisesti `Locale.getDefault()` -arvoa käyttöjärjestelmämme kielelle, mutta voimme myös antaa `Locale` -arvon haluamamme kielen mukaan.

Voimme myös käyttää `toCharArray()` -metodia ja `CharSequence.map()` -funktiota muuttaaksemme kirjaimet pienaakkosiksi ja yhdistää ne takaisin merkkijonoksi, mutta tämä on hieman monimutkaisempi tapa.

## Katso myös

- [Kotus-sivusto merkkijonojen muuttamisesta pienaakkosiksi](https://www.kotus.fi/nyt/kayttomanualit/uudenkielennayttajalle/merkkijonojen-muuttaminen-pienaakkosiksi) 
- [Kotlin Docs - String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)