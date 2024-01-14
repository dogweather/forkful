---
title:                "Kotlin: Alaryhdyksien erottaminen"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: 

Miksi joku haluaisi käyttää substringsien poimintaa? Onko se välttämätöntä tai hyödyllistä ohjelmoinnissa? 

Yksi syy voisi olla tehostaa koodia ja tehdä siitä helpommin luettavaa. Substringsien poimiminen tarkoittaa, että voit erottaa tarvittavan tiedon suuremmasta merkkijonosta ja käsitellä sitä erillisenä osana. Tämä on tärkeää, kun haluat käsitellä vain tietyn osan merkkijonosta, eikä koko merkkijonoa.

## Miten:

```Kotlin
val merkkijono = "Tämä on esimerkkimerkkijono"
val alkuSubString = merkkijono.substring(0, 4)
println(alkuSubString)

// Output: Tämä
```

Kotlinin `substring()`-funktio hyväksyy kaksi parametria; alkuindeksin ja loppuindeksin. Näiden avulla voit määrittää, mistä kohdasta haluat poimia substringsin. Alkuindeksi alkaa 0:sta ja loppuindeksi on merkkijonon pituus vähennettynä yhdellä.

Voit myös käyttää `substringAfter()` ja `substringBefore()` funktioita, jos haluat poimia merkkijonon tietyn kohdan jälkeen tai ennen.

```Kotlin
val merkkijono = "Hyvää päivää, maailma!"
val jälkeenSubstring = merkkijono.substringAfter(",")
println(jälkeenSubstring)

// Output: maailma!
```

## Syvempi sukellus: 

Substringsien poimiminen on hyödyllistä myös silloin, kun käsitellään syötteitä käyttäjältä. Voit esimerkiksi pyytää käyttäjää syöttämään puhelinnumeron ja sen jälkeen poimia numerot ja tarkistaa, ovatko ne oikeassa muodossa.

Lisäksi `substring()`-funktio on hyödyllinen silloin, kun haluat luoda uusia merkkijonoja. Voit esimerkiksi luoda uuden sähköpostiosoitteen lisäämällä käyttäjän nimen jonon loppuun.

## Katso myös:

- [Kotlinin viralliset dokumentaatiot substringien poimimisesta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Kotlin String API Cheat Sheet](https://medium.com/@harivigneshjayapalan/kotlin-string-api-cheat-sheet-part1-a818c0db5d16)
- [Kotlin Quick Reference Guide](https://www.programiz.com/kotlin/substring)