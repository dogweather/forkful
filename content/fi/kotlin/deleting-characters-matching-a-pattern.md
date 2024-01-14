---
title:                "Kotlin: Alla olevaa kuvioita vastaavien merkkien poistaminen"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi
On monia syitä, miksi haluaisit poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Ehkä haluat puhdistaa käyttäjän syötteen tiettyjen sääntöjen mukaisesti tai haluat yksinkertaistaa tietojen käsittelyä.

## Miten
Kotlinin avulla merkkien poistaminen, jotka vastaavat tiettyä kaavaa, on helppoa. Voit käyttää `replace` -metodia merkkijonon muokkaamiseksi ja antaa sille parametrina regex-kaavan. Tässä on yksinkertainen esimerkki:

```Kotlin
val sana = "Hei maailma!"
val poistettava = "[aeiou]"
val uusiSana = sana.replace(poistettava.toRegex(), "")
// Output: H ml!
```

Voit myös antaa parametrina regex-merkkijonon sijaan `Regex` -olion ja siten tehdä koodista hieman suoraviivaisemman:

```Kotlin
val sana = "Hello world!"
val poistettava = Regex("[aeiou]")
val uusiSana = sana.replace(poistettava, "")
// Output: H ll wrld!
```

## Syväsukellus
Regex-kaava voi sisältää monimutkaisia sääntöjä, kuten säännöllisiä lausekkeita ja metakaraktereita. Esimerkiksi `"[0-9]"` vastaa mihin tahansa numeroon ja `"."` vastaa mihin tahansa merkkiin. Voit myös käyttää erilaisia metakaraktereita, kuten `"\d"` vastaamaan numeroon, `"\w"` vastaamaan kirjaimiin ja numeroihin ja `"\s"` vastaamaan välilyöntiin. Voit tutustua kaikkiin käytettävissä oleviin metakaraktereihin [täältä](https://developer.android.com/reference/java/util/regex/Pattern#characters).

Voit myös lisätä monimutkaisempia sääntöjä kaavaasi, kuten `"[a-z]+@[a-z]+\.[a-z]+"` vastaamaan kelvollisia sähköpostiosoitteita. Voit kokeilla erilaisia kaavoja ja poistaa merkkejä joustavasti käyttämällä Kotlinin `Lang` -kirjastoa.

## Katso myös
- [String.replace() - Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/replace.html)
- [Regex - Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Regular Expressions - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex Cheat Sheet - Debuggex](https://www.debuggex.com/cheatsheet/regex/javascript)