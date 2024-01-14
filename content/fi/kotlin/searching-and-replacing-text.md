---
title:                "Kotlin: Tekstin hakeminen ja korvaaminen"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Ehkä olet huomannut, että tekstiä joudutaan usein korvaamaan erilaisilla ohjelmointiprojekteissa. Se voi olla uuden määritelmän lisääminen tietokannan kyselyyn tai vain halu muuttaa tietyn sanan muotoilua. Harkitset sitten tekstieditoria tai koodieditoria, manuaalinen korvaaminen voi olla hidasta ja altis virheille. Onneksi Kotlin tarjoaa tehokkaan ja helpon tavan etsiä ja korvata tekstiä ohjelmallisesti.

## Miten

Kotlinin String-tyypillä on valmiiksi määriteltyjä toimintoja, jotka helpottavat tekstien etsimistä ja korvaamista. Yksi näistä toiminnoista on `replace`-metodi, joka korvaa kaikki annetut merkkijonot toisella merkkijonolla. Esimerkiksi:

```Kotlin
val teksti = "Tämä on vain esimerkki"
val uusiTeksti = teksti.replace("vain", "ainoastaan")
println(uusiTeksti)
```

Tulostus:

```
Tämä on ainoastaan esimerkki
```

Voit myös antaa parametrina regex-lausekkeen, jolloin voit hakea ja korvata esimerkiksi erilaisia muotoiluja. Esimerkiksi:

```Kotlin
val teksti = "Käytä email@example.com työpaikan hakemiseen"
val uusiTeksti = teksti.replace("[a-z0-9\\.\\-]+@[a-z0-9\\.\\-]+".toRegex(), "**********")
println(uusiTeksti)
```

Tulostus:

```
Käytä ********** työpaikan hakemiseen
```

## Syväsukellus

Kotlin tarjoaa myös muita tapoja etsiä ja korvata tekstiä, kuten `replaceFirst` ja `replaceBefore` -metodit, jotka korvaavat vain ensimmäisen löydetyn osuman tai korvaavat tekstin ennen tai jälkeen tietyn merkkijonon. Näitä kaikkia metodeja voi käyttää myös StringBuilder-tyypillä, jolloin pystyt muokkaamaan ja korvaamaan tekstiä vielä tehokkaammin.

Kun ohjelmoit Kotlinilla, sinun ei tarvitse enää tehdä etsimistä ja korvaamista manuaalisesti - voit hyödyntää kielen valmiiksi sisäänrakennettuja toimintoja ja säästää aikaa ja vaivaa.

## Katso myös

- [Kotlinin virallinen dokumentaatio String-tyypin metodeista](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [Ohjeet regex-lausekkeiden käyttöön Kotlinissa](https://www.regular-expressions.info/kotlin.html)
- [Kotlinin virallinen verkkosivusto](https://kotlinlang.org/)