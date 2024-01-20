---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Muunnetaan merkkijono pieniksi kirjaimiksi - toisin sanoen, muutetaan kaikki merkkijonon sisältämät iso kirjaimet pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän tekstin normalisoimiseksi, esimerkiksi kun vertaillaan merkkijonoja tai etsitään avainsanoja jne.

## Miten:
```Kotlin
val isoMerkkijono = "Suomi"
val pieniMerkkijono = isoMerkkijono.toLowerCase()

println(isoMerkkijono) // Tulostaa: Suomi
println(pieniMerkkijono) // Tulostaa: suomi
```

## Syvempi tutkiskelu
Historiallisesti ottaen pienaakkosten käyttöön on johtanut useita tekijöitä, kuten yksinkertaistaminen ja yhdenmukaistaminen. Kotlinissa "toLowerCase()" -funktiota voi käyttää vaihtoehtoiseen tapaan muuntaa merkkijono pieniksi kirjaimiksi. Tämä funktio soveltaa Unicode Case Folding -sääntöä, joka on vakiomuoto sen mukaan, kuinka isot ja pienet kirjaimet korreloivat. Tämä on tärkeää, koska se sallii merkkijonon välinpitämättömän vertailun eri kielialueilla ja merkistöissä.

## Katso lisäksi
Viralliset Kotlin-dokumentit ovat mahtava resurssi syventyvä tutkimus: [Kotlin Dokumentaatio](https://kotlinlang.org/docs/)

Tarkista myös tämä artikkeli, joka sisältää monia esimerkkejä ja neuvoja Kotlinin kanssa: [Kotlinin perusteet](https://www.programiz.com/kotlin-programming)