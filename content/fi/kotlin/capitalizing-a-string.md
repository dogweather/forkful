---
title:    "Kotlin: Merkkijonon kirjoittaminen isolla alkukirjaimella"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi? Tämä voi olla hyödyllistä esimerkiksi silloin, kun käsittelet käyttäjältä syötettyjä sanoja tai lauseita ja haluat varmistaa niiden oikeanlaisen muotoilun.

## Miten
Merkkijonon ensimmäisen kirjaimen isoksi muuttamiseksi Kotlinissa voit käyttää sisäänrakennettua capitalize() -funktiota. Se ottaa merkkijonon ja palauttaa uuden merkkijonon, jossa ensimmäinen kirjain on muutettu isoksi. Alla on esimerkki koodista ja sen tulostus.

```Kotlin
val sana = "tervehdys"
val uusiSana = sana.capitalize()

println(uusiSana)

// Tulostaa "Tervehdys"
```

## Syvällinen tarkastelu
Voit myös suorittaa capitalize()-funktion itse, rakentamalla oman koodin sen ympärille. Tämä voi olla hyödyllistä, jos haluat esimerkiksi muuttaa useamman kuin yhden kirjaimen isoksi tai ottaa huomioon erilaiset kielisäännöt. Alla on esimerkki toiminnasta, jossa funktio noudattaa englannin kielen sääntöjä.

```Kotlin
fun capitalize(word: String): String {
    val firstChar = word[0].toUpperCase()
    val restOfWord = word.substring(1, word.length).toLowerCase()
    return firstChar + restOfWord
}

val lause = "tämä on esimerkki"
val uusiLause = lause.split(" ")
    .map { capitalize(it) } 
    .joinToString(" ")

println(uusiLause)

// Tulostaa "Tämä On Esimerkki"
```

## Katso myös
- [Kotlin - Merkkijonon muotoilu](https://kotlinlang.org/docs/reference/strings.html#string-formatting)
- [Stack Overflow - How to capitalize the first letter of a string in Kotlin](https://stackoverflow.com/questions/6699115/how-to-capitalize-the-first-character-of-a-string-in-java)
- [Baeldung - Capitalizing Strings in Kotlin](https://www.baeldung.com/kotlin/capitalize-strings)