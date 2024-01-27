---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Merkkijonon muuttaminen isokirjaimiseksi tarkoittaa, että kaikki kirjaimet muutetaan suuriksi kirjaimiksi. Koodarit tekevät tätä usein käyttöliittymiä luodessaan tai tietoja esittäessään, jotta teksti erottuisi selkeämmin.

## How to:
Kotlinissa stringin muuttaminen isokirjaimiseksi on helppoa. Katsotaan esimerkki:

```Kotlin
fun main() {
    val original = "moi maailma"
    val capitalized = original.uppercase()
    println(capitalized)
}
```

Tuloste:
```
MOI MAAILMA
```

## Deep Dive
Kotlinkielisessä ohjelmoinnissa `.uppercase()` on virallinen tapa muuttaa merkkijono isokirjaimiseksi. Historiallisesti `.toUpperCase()` oli käytössä, mutta Kotlin 1.5 alkaen `.uppercase()` on suositeltu, sillä se tukee paremmin erilaisia kulttuurikohtaisia sääntöjä kirjainten käsittelyssä.

Vaihtoehtoisia tapoja on, kuten `.capitalize()`, joka tekee isoksi vain ensimmäisen kirjaimen, tai käyttäen `java.util.Locale`:a tarkentaakseen kirjainten isontamista kulttuurisidonnaisesti:

```Kotlin
val capitalizedWithLocale = original.uppercase(Locale("fi", "FI"))
```

Merkkijonojen isontamisen käytäntö on kuitenkin suoraviivaista. Kotlinin sisäisesti `uppercase()` käyttää Java Standard Libraryn toimintoja.

## See Also
Kotlinin virallinen dokumentaatio `.uppercase()` -metodista:
[Kotlin Documentation: uppercase](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/uppercase.html)

Java `Locale`-luokasta:
[Java Locale Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)

Unicode-standardit kirjaimille ja isontamiselle:
[Unicode Case Folding](https://www.unicode.org/reports/tr21/tr21-5.html)
