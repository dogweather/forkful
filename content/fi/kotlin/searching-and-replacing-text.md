---
title:                "Kotlin: Tekstin Etsiminen ja Korvaaminen"
simple_title:         "Tekstin Etsiminen ja Korvaaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi: Miksi etsiä ja korvata tekstiä?

Joskus ohjelmoinnissa on tarpeen löytää ja korvata tietyt tekstit tiedostoista. Tämä voi johtua esimerkiksi tietokonetyöskentelyn automatisointitarpeista tai suurien tekstimäärien muokkaamisesta. Onneksi Kotlin tarjoaa näppärän tavan toteuttaa tämä.

## Kuinka: Etsi ja korvaa tekstin Kotlinilla

Etsi ja korvaa tekstin toiminnallisuus on osa Kotlinin standardikirjastoa, joten sen käyttö on helppoa. Käytämme tätä toiminnallisuutta String-luokan extension-metodin avulla.

```Kotlin
val originalText = "Tervetuloa Kotlin-maailmaan!" 
val newText = originalText.replace("Kotlin", "mahtava") 
println(newText)
```

Tämä koodi tulostaa "Tervetuloa mahtava-maailmaan!". Voit myös käyttää säännöllisiä lausekkeita etsiessäsi ja korvatessaessasi tekstiä:

```Kotlin
val originalText = "Viikonlopun säätila: Lämpötila: 23C, Pilvisyys: Osittain pilvinen" 
val newText = originalText.replace(Regex("Lämpötila: (\\d+)C"), "Lämpötila: 25C") 
println(newText)
```

Tämä koodi tulostaa "Viikonlopun säätila: Lämpötila: 25C, Pilvisyys: Osittain pilvinen". Huomaat, että säännöllistä lauseketta käytetään replace-metodin toisessa parametrissa.

## Syventävä tarkastelu: Huomioita etsimisestä ja korvaamisesta Kotlinissa

Olet ehkä huomannut, että replace-metodi luo uuden merkkijonon eikä muuta alkuperäistä. Tämä on tärkeää huomioida erityisesti, jos käytät replace-metodia silmukassa ja haluat muuttaa alkuperäistä merkkijonoa. Tämä voidaan kuitenkin ratkaista helposti käyttämällä mutable-luokkia, kuten StringBuilder tai StringBuffer.

Lisäksi, huomaathan, että replace-metodin ensimmäinen parametri voi olla myös säännöllinen lauseke, ei pelkästään merkkijono. Tämä tarjoaa enemmän joustavuutta etsintä- ja korvausprosessissa.

## Katso myös

- [Kotlinin virallinen dokumentaatio etsimisestä ja korvaamisesta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Tutoriaali: Regex-säännölliset lausekkeet Kotlinissa](https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm)
- [Kotlinin muuta hyödyllistä String-extension-metodia](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html)