---
title:                "Kotlin: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi Stringin muuntaminen pienaakkosiin on hyödyllistä?

Stringin muuntaminen pienaakkosiin on tärkeä osa ohjelmointia, koska se helpottaa tekstin käsittelyä ja vertailua. Pienaakkosten käyttö yhdenmukaistaa tekstin, mikä tekee siitä helpommin luettavan ja ymmärrettävän.

## Kuinka muuntaa String pienaakkosiin Kotlinissa?

```Kotlin
val sana = "TEKSTI"
println(sana.lowercase()) //tulostaa "teksti"
```

Stringin muuntaminen pienaakkosiin Kotlinissa on yksinkertaista, sillä siihen on sisäänrakennettu metodi ```lowercase()```. Metodi palauttaa uuden String-olion, joka sisältää kaikki kirjaimet pienaakkosina.

Jos haluat muuntaa Stringin pienaakkosiin ilman uuden olion luomista, voit käyttää ```lowercaseInPlace()``` -metodia:

```Kotlin
var tekstiMuutettavaksi = "MUUTETTAVA TEKSTI"
tekstiMuutettavaksi.lowercaseInPlace()
println(tekstiMuutettavaksi) //tulostaa "muutettava teksti"
```

Tässä tapauksessa alkuperäinen String-muuttuja muutetaan suoraan pienaakkosiksi.

## Syvempi sukellus Stringin muuntamiseen pienaakkosiin

Kotlinin ```lowercase()``` ja ```lowercaseInPlace()``` -metodien toiminta perustuu Unicode-standardissa määritettyyn pienaakkosten muuntamiseen. Unicode-standardi sisältää kaikki mahdolliset merkit ja kirjaimet, ja sen avulla varmistetaan, että ohjelmointikielet käyttävät yhteistä merkistöä.

Usein ohjelmointikielistä löytyy myös muita metodeja, kuten ```toLowercase()```, jotka suorittavat saman toiminnon. Näissä tapauksissa kannattaa tarkistaa, miten kyseinen metodi käsittelee erikoismerkkejä ja kielikohtaisia eroavaisuuksia.

On myös hyvä ottaa huomioon, että merkkijonon pienaakkosiksi muuttaminen ei yleensä vaikuta merkkijonon sisältöön. Esimerkiksi sana "ÄITI" muuttuu pienaakkosiin "äiti", mutta kirjoitusasu ei muutu. Jos haluat muuttaa myös sanojen kirjoitusasun, voit käyttää esimerkiksi metodeja ```capitalize()``` tai ```titlecase()```.

## Katso myös

- Kotlinin virallinen dokumentaatio: https://kotlinlang.org/docs/reference/strings.html#string-literals
- Unicode-standardin tietoja: https://unicode.org/
- Vinkkejä merkkijonojen käsittelyyn Kotlinissa: https://www.baeldung.com/kotlin/strings-processing