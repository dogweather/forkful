---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi."
html_title:           "Kotlin: Merkkijonon muuttaminen isoiksi kirjaimiksi."
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Miksi pääoman käyttöönotto merkkijonossa?

Olet todennäköisesti jo nähnyt, että jotkut ohjelmat esimerkiksi muutavat otsikot ja virkkeiden alkukirjaimet isoihin kirjaimiin. Tätä kutsutaan merkkijonon pääoman käyttöönotoksi ja se on yksi tärkeä työkalu ohjelmoinnissa. Se ei yksinkertaisesti vain tee tekstistä kauniimpaa, vaan sillä voi myös olla merkittävä rooli ohjelman toiminnallisuudessa.

## Kuinka tehdä pääoman käyttöönotto Kotlin-kielellä?

Kotlinilla on sisäänrakennettu `capitalize()`-funktio, joka tekee merkkijonon ensimmäisestä kirjaimesta ison kirjaimen ja muut kirjaimet pieniksi. Voit käyttää tätä funktiota seuraavasti:

```Kotlin
val sana = "kotlin"
val isoinaKirjaimina = sana.capitalize()

println(isoinaKirjaimina)

// output: Kotlin
```

Voit myös käyttää `toUpperCase()`-funktiota, joka muuttaa kaikki merkit isoina kirjaimina:

```Kotlin
val sana = "kotlin"
val isoinaKirjaimina = sana.toUpperCase()

println(isoinaKirjaimina)

// output: KOTLIN
```

## Syventävä tieto pääoman käyttöönotosta

Pääoman käyttöönotto on tärkeä osa merkkijonojen käsittelyä ohjelmoinnissa. Se voi auttaa esimerkiksi käyttäjän syöttämän tiedon oikeellisuuden tarkistamisessa, sillä monet käyttäjät eivät välttämättä muista kirjoittaa kaikki isot kirjaimet oikeisiin kohtiin. Lisäksi pääoman käyttöönotto on hyödyllinen esimerkiksi vertaillessa merkkijonoja, sillä isot ja pienet kirjaimet voivat vaikuttaa vertailun tulokseen.

## Katso myös

- [Kotlinin dokumentaatio merkkijonojen käsittelystä](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/java.lang.-string/)
- [Kotlinin pääoman käyttöönotto Stack Overflow:ssa](https://stackoverflow.com/questions/33003752/how-to-capitalize-the-first-letter-of-a-string-in-kotlin)