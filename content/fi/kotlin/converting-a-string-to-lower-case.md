---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Kotlin: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

On monia käyttötarkoituksia, miksi haluat muuttaa merkkijonon pieniksi kirjaimiksi. Ehkä haluat vertailla merkkijonoja ilman, että välitetään kirjainten koosta tai ehkä haluat yhdenmukaistaa merkkijonojen muotoilun.

## Miten

### Esimerkki 1: Yksinkertainen muunnos

Käyttäen Kotlinin String-luokan toLowerCase()-metodia voit helposti muuttaa merkkijonon pieniksi kirjaimiksi.

```Kotlin
val s = "TÄMÄ ON ESIMERKKI"
println(s.toLowerCase())
```

Tuloste: tämä on esimerkki

### Esimerkki 2: Muunnos jossain tiettynä paikassa

Voit myös valita haluamasi kohdan merkkijonosta ja muuttaa vain sen pieniksi kirjaimiksi.

```Kotlin
val s = "Muista Muuttaa Merkkin Tänne"
println(s.toLowerCase(7..11))
```

Tuloste: Muista muuttaa merkkiä tänne

## Syvempi sukellus

Kotlinin String-luokka on muokattavissa oleva, joten se ei tarjoa sisäänrakennettua konversiota isojen ja pienten kirjainten välillä. Sen sijaan, se käyttää Unicode-standardia, joka määrää, mikä merkki tulee ensin ja mikä viimeiseksi.

Unicode-standardin mukaan suuret kirjaimet tulevat aakkoston alkuun ja pienet kirjaimet aakkoston loppuun. Tämä on syy sille, miksi konversio on "teknisesti" mahdollista.

## Katso myös

- [Kotlinin String-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Unicode-standardi](https://unicode.org/)