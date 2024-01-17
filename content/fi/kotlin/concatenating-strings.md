---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Kotlin: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Konkatenaatio eli merkkijonojen yhdistäminen on ohjelmoinnissa yleinen tapa yhdistää kaksi tai useampaa merkkijonoa yhdeksi kokonaisuudeksi. Tätä tarvitaan esimerkiksi tekstin luomisessa, jolloin halutaan lisätä muuttujiin tallennettuja arvoja halutussa järjestyksessä.

## Kuinka:

Kotlinissa merkkijonojen konkatenaatio tapahtuu käyttämällä plus-merkkiä (+) kahden tai useamman merkkijonon välissä. Katso esimerkki alla:

```kotlin
val etunimi = "Matti"
val sukunimi = "Mäkinen"
println(etunimi + " " + sukunimi)

// Output: Matti Mäkinen
```

Merkkijonon lisäksi plus-merkkiä voidaan käyttää myös muuttujan ja tekstin yhdistämiseen. Katso esimerkki alla:

```kotlin
val sivujenMaara = 315
println("Kirjassa on " + sivujenMaara + " sivua.")

// Output: Kirjassa on 315 sivua.
```

## Syväluotaus:

Merkkijonojen konkatenaatio ei ole uusi keksintö, vaan sitä on käytetty ohjelmoinnissa jo pitkään. Aikaisemmin se tehtiin yleensä käyttämällä esimerkiksi C-kielen strcat-funktiota tai Java-kielen StringBuilder-luokkaa.

Kotlinissa konkatenaatio on toteutettu tehokkaasti sisäisen StringBuilder-luokan avulla. Tämä mahdollistaa suorituskyvyn optimoinnin ja tekee koodista helpommin luettavaa.

## Katso myös:

- [Kotlinin dokumentaatio merkkijonojen konkatenaatiosta](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java StringBuilder-luokan dokumentaatio](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html)
- [C strcat-funktion dokumentaatio](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)