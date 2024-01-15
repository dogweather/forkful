---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Kotlin: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

### Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat hyödyllisiä, jos haluat hakea ja muokata tekstiä tietyin kriteerein. Ne voivat säästää aikaa ja vaivaa, kun käsittelet suuria määriä tekstiä tai haluat tarkasti etsiä tiettyjä sanoja tai ilmauksia. Säännöllisiä lausekkeita voi myös käyttää monissa ohjelmointikielissä, joten niiden osaaminen on hyödyllistä monessa tilanteessa.

### Kuinka käyttää säännöllisiä lausekkeita Kotlinissa

Säännöllisten lausekkeiden käyttäminen Kotlinissa on helppoa ja vaivatonta. Voit aloittaa käyttämällä Regex-luokkaa, joka tarjoaa valmiita toimintoja säännöllisiin lausekkeisiin. Voit luoda Regex-olion antamalla sille säännöllisen lausekkeen merkkijonona. Esimerkkinä voit etsiä kaikki sanat, jotka alkavat kirjaimella "a" ja päättyvät kirjaimeen "i", käyttämällä seuraavaa koodia:

```Kotlin
val pattern = Regex("a[a-zA-Z]*i")
val text = "Tämä on esimerkki, jossa on sana abbai."
val result = pattern.find(text)
println(result?.value)
```

Tämä tulostaa "abbai", koska se on ainoa sana, joka vastaa säännöllistä lauseketta. Voit myös käyttää Regex-luokan muita metodeita, kuten esimerkiksi `matches()`, `replace()` ja `findAll()`, joiden avulla voit muokata ja hakea tekstiä haluamallasi tavalla.

### Syvempi sukellus säännöllisiin lausekkeisiin

Säännöllisiä lausekkeita voi käyttää monella eri tavalla ja niiden mahdollisuudet ovat lähes rajattomat. Voit esimerkiksi käyttää ns. "capture groups" -toimintoa, joka mahdollistaa tietyn osan tekstin tallentamisen. Tämä on hyödyllistä esimerkiksi silloin, kun haluat muokata tekstin osaa ja palauttaa sen uudelleen muokattuna.

Säännöllisiä lausekkeita käytettäessä on myös tärkeää huomioida suorituskyky. Monimutkaiset säännölliset lausekkeet voivat hidastaa ohjelman suoritusta, joten kannattaa tutkia erilaisia vaihtoehtoja ja valita yksinkertaisin mahdollinen ratkaisu.

### Katso myös

- [Kotlinin virallinen Regex-dokumentaatio](https://kotlinlang.org/docs/regular-expressions.html)
- [Regex101 - Verkkosivusto, jossa voit testata ja kehittää säännöllisiä lausekkeita](https://regex101.com/)
- [10 hyvää syytä käyttää säännöllisiä lausekkeita ohjelmoinnissa](https://www.codeindustry.net/10-good-reasons-to-use-regular-expressions/)