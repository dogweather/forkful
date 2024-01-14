---
title:    "Kotlin: Nykyisen päivämäärän saaminen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi
Miksi joku haluaisi selvittää ajantasaisen päivämäärän? Aluksi se saattaa tuntua yksinkertaiselta ja tarpeettomalta tehtävältä, mutta todellisuudessa ajantasaisen päivämäärän selvittäminen on välttämätöntä monissa ohjelmoinnin sovelluksissa. Esimerkiksi kalenteri-, varaus- tai tapahtumienhallintasovellukset tarvitsevat tietoa nykyisestä päivästä toimiakseen oikein.

## Kuinka
Ajan ja päivämäärän selvittäminen Kotlinilla onnistuu helposti käyttämällä `LocalDate.now()` -metodia. Tämä palauttaa ajantasaisen päivämäärän `LocalDate`-luokan tapauksessa. Voit myös käyttää muita `now()` -metodia sisältäviä luokkia, kuten `LocalDateTime` ja `LocalTime`. Alla on esimerkki, jossa ajantasainen päivämäärä tallennetaan muuttujaan ja tulostetaan konsoliin:

```Kotlin
val nykyinenPaiva = LocalDate.now()
println(nykyinenPaiva)
```

Tämä koodi tulostaa konsoliin `2021-04-08`, mikä on tämänhetkinen päivämäärä tämän artikkelin kirjoittamisen aikaan.

## Syvenny
Jos haluat tietää tarkemmin, miten Kotlinin `now()` -metodi toimii, voit syventyä `LocalDate`-luokan dokumentaatioon. Siellä voit nähdä muita käytettävissä olevia metodeja ja niiden toimintaa.

Jokainen `now()` -metodilla tehty kutsu palauttaa aina uuden, ajantasaisen päivämäärän. Tämä saattaa aiheuttaa ongelmia, jos haluat käyttää samaa päivämäärää useaan kertaan koodissasi. Tässä tapauksessa voit tallentaa päivämäärän muuttujaan ja käyttää sitä tarvittaessa.

## Katso myös
- [Kotlin LocalDate -dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Kotlin LocalDateTime -dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date-time/)
- [Kotlin LocalTime -dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlintime/-local-time/)