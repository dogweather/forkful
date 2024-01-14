---
title:    "Kotlin: Merkkijonon pituuden löytäminen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? Yleisimpiä syitä ovat mahdollisuus tarkistaa käyttäjän antamaa syötettä tai varmistaa, että merkkijono mahtuu määritettyyn muuttujan kokoon.

## Kuinka

Merkkijonon pituuden selvittäminen Kotlinissa on yksinkertaista. Voit käyttää `length`-metodia, joka palauttaa merkkijonon pituuden.

```Kotlin
val merkkijono = "Hei!"
println(merkkijono.length)
// Output: 4
```

Voit myös käyttää `count()`-funktiota, joka palauttaa saman tuloksen.

```Kotlin
val merkkijono = "Tämä on merkkijono"
println(merkkijono.count())
// Output: 19
```

## Syvällinen sukellus

Kotlinin merkkijonon pituuden selvittämisessä on hyvä muistaa, että se palauttaa merkkien määrän, ei sanojen. Esimerkiksi välilyönnit lasketaan myös merkkeinä.

```Kotlin
val merkkijono = "Tämä on testi"
println(merkkijono.length)
// Output: 13
println(merkkijono.split(" ").size)
// Output: 4
```

Lisäksi merkkijonon pituutta ei voi muuttaa, sillä merkkijonot ovat niin sanottuja "immutable"-muuttujia, jotka eivät voi muuttaa arvoaan.

## Katso myös

- [Kotlinin dokumentointi: merkkijonot](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Stack Overflow: merkkijonon pituuden selvittäminen](https://stackoverflow.com/questions/41627592/kotlin-how-to-determine-string-length)