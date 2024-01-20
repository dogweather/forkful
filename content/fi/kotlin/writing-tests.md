---
title:                "Testien kirjoittaminen"
html_title:           "Kotlin: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Testien kirjoittaminen on tärkeä osa ohjelmoinnin prosessia, jossa koodin toiminnallisuutta tarkastellaan ja varmistetaan sen oikeellisuus. Testien avulla ohjelmoijat voivat välttää virheitä ja varmistua siitä, että heidän koodinsa toimii odotetusti erilaisissa tilanteissa.

## Miten:

```Kotlin
fun sum(x: Int, y: Int): Int { 
  return x + y 
}

fun testSum() {
  val result = sum(2, 3) // kutsutaan funktiota
  assert(result == 5) // tarkistetaan, että tulos on odotettu
}

// Suoritetaan testi
testSum()
```

Tässä esimerkissä on määritelty funktio, joka summaa kaksi kokonaislukua. Sen jälkeen on määritelty testi, joka kutsuu tätä funktiota ja tarkistaa, että saatu tulos on odotettu. Testin suorittaminen antaa meille varmuuden siitä, että funktio toimii oikein ja tekee halutunlaisen laskutoimituksen.

## Syväluotaus:

Testien kirjoittaminen on keino varmistaa ohjelman toiminnallisuus ja löytää mahdolliset virheet jo ennen kuin ohjelma julkaistaan. Testien lisäksi on olemassa myös muita tapoja varmistaa koodin laatu, kuten koodin tarkastaminen ja käsin tehtävä debuggaus. On tärkeää löytää oma tapa testata ja varmistaa oman koodin toimivuus.