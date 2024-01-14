---
title:    "Kotlin: Nauhan pituuden löytäminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi henkilö voisi haluta selvittää merkkijonon pituuden? Merkkijonojen käsittely on tärkeä osa ohjelmointia ja usein tarvitsemme tietoa merkkijonojen koosta ja sisällöstä. Merkkijonon pituuden selvittäminen on yksi tapa tehdä tämä.

## Kuinka

```Kotlin
// Luodaan esimerkki-merkkijono
val merkkijono = "Tervetuloa Kotlinin ihmeelliseen maailmaan"

// Käytetään String-luokan metodia length()
val pituus = merkkijono.length

println(pituus) // Tulostaa: 42
```

Deep Dive:

Merkkijonojen pituuden selvittämiseksi voimme käyttää String-luokan length-metodia, kuten yllä olevassa esimerkissä. Tämä metodi palauttaa arvon, joka kertoo merkkijonon merkkien lukumäärän. On hyvä huomata, että välilyönnit ja muut erikoismerkit lasketaan mukaan pituuteen.

##Lue Lisää

- [Kotlinin virallinen ohjelmointikielen peruskäsitteet](https://kotlinlang.org/docs/reference/basic-syntax.html)
- [Merkkijonojen käsittely Java- ja Kotlin-ohjelmointikielissä](https://www.baeldung.com/java-string-length)
- [String-luokan dokumentaatio Kotlinissa](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)

## Katso myös

- [Kotlinin opetusohjelma aloittelijoille](https://kotlinlang.org/docs/tutorials/getting-started.html)
- [Kotlin-ohjelmointikielen virallinen sivusto](https://kotlinlang.org/)