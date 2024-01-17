---
title:                "Alimerkkijonojen poimiminen"
html_title:           "Kotlin: Alimerkkijonojen poimiminen"
simple_title:         "Alimerkkijonojen poimiminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Substringien erottaminen tarkoittaa tietyn merkkijonon osan erottamista suuremmasta merkkijonosta. Ohjelmoijat tekevät tätä usein, kun he tarvitsevat tietyn osan merkkijonosta esimerkiksi käsittelyä tai analysointia varten.

## Kuinka:
```
// Luodaan esimerkkimerkkijono
val sana = "Kotlin on hauska ohjelmointikieli"

// Erotetaan ensimmäiset viisi merkkiä
val alku = sana.substring(0,5)
println(alku)

// Tulostaa: Kotlin
```

```
// Luodaan esimerkkimerkkijono
val kaupunki = "Helsinki, Suomi"

// Erotetaan viimeinen osa kaupungista
val maa = kaupunki.substring(9)
println(maa)

// Tulostaa: Suomi
```

## Syväluotaus:
Substringien erottaminen on tapahtunut ohjelmoinnissa jo vuosikymmenien ajan. Aikaisemmin tämä tehtiin manuaalisesti, mutta nykyään eri ohjelmointikielillä on valmiita funktioita tähän tarkoitukseen. Joissakin kielissä, kuten C:ssä, substringien erottamiseen käytetään merkkijonofunktioita, kun taas toisissa kielissä, kuten Pythonissa, käytetään indeksointia merkkijonon sisällä.

## Katso myös:
- [Substringien erottaminen Kotlin-documentaatiosta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Merkkijonofunktiot C:ssä](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Indeksointi Pythonissa](https://www.w3schools.com/python/python_strings.asp)