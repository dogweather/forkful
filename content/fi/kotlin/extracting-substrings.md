---
title:    "Kotlin: Alimerkkijonojen erottelu"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi substringsien poimiminen on hyödyllistä ohjelmoinnissa?

Substringsien poimiminen, eli lyhyempien merkkijonojen erottaminen pidemmistä, voi olla hyödyllistä monessa eri tilanteessa. Esimerkiksi tietokantaoperaatioita tehtäessä, tiedon käsittelyssä ja käyttöliittymän toiminnallisuuksien toteuttamisessa substringsien poimiminen voi helpottaa ja nopeuttaa koodin toimintaa.

## Kuinka poimia substrings "Kotlin" koodiesimerkkien avulla?

Substringsien poimiminen Kotlinissa on helppoa ja nopeaa käyttämällä merkkijonon "substring()" funktiota. Se ottaa parametreikseen aloitus- ja lopetusindeksit, ja palauttaa näiden välissä olevan osan merkkijonosta.

```Kotlin
val sana = "Tämä on esimerkki"
val osa = sana.substring(8, 12)

println(osa) // "esim"
```

Kuten esimerkissä nähdään, substringin käyttöönotto lyhentää koodin määrää ja tekee siitä selkeämpää.

## Näin otat vielä syvemmän sukelluksen substringsien maailmaan.

Kotlin tarjoaa myös muita tapoja poimia substringsia, kuten käyttämällä "substringBefore()" ja "substringAfter()" funktioita. Nämä otetaan käyttöön samalla tavalla kuin "substring()" funktiossa, mutta ne ottaa parametrikseen merkkijonon, jonka mukaan splitataan merkkijono ennen tai jälkeen tiettyä kohtaa. Esimerkiksi:

```Kotlin
val sana = "Tämä on esimerkki"
val ennen = sana.substringBefore("on")
val jälkeen = sana.substringAfter("on")

println(ennen) // "Tämä"
println(jälkeen) // " esimerkki"
```

Lisäksi Kotlin tarjoaa myös "split()" funktion, joka ottaa substringsien poimimisen vielä askeleen pidemmälle. Tämä funktio ottaa parametrikseen merkkijonon, jonka mukaan jaetaan alkuperäinen merkkijono osiin.

```Kotlin
val lause = "Tämä on hyvä esimerkki joka on jaettu osiin"
val osat = lause.split(" ")

println(osat) // ["Tämä", "on", "hyvä", "esimerkki", "joka", "on", "jaettu", "osiin"]
```

Kaiken kaikkiaan substringsien poimiminen on siis erittäin hyödyllistä ja monipuolista Kotlinissa.

## Katso myös

- [Substringin dokumentaatio Kotlinin virallisilla verkkosivuilla](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Kotlin oppimateriaalit mielenkiintoisten ohjelmointihaasteiden ratkaisemiseksi](https://kotlinlang.org/docs/tutorials/)