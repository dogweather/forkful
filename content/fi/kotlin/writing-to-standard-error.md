---
title:                "Kotlin: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat kirjoittaa tietoja standard erroriin ohjelmassasi. Tärkein syy on se, että se auttaa sinua tunnistamaan ja korjaamaan virheitä ohjelmassasi. Standard erroria käytetään myös usein raportoimaan tärkeitä tapahtumia ohjelmassa, kuten poikkeuksia tai varoituksia.

## Miten

Kotlin-kielen avulla standard erroriin kirjoittaminen on helppoa. Seuraavassa on esimerkkejä siitä, miten voit kirjoittaa standard erroriin erilaisia tietoja ohjelmassasi:

```Kotlin
fun main() {
    //Kirjoittaa merkkijonon standard erroriin
    System.err.println("Tämä on virheen viesti.")

    //Kirjoittaa virheen standard erroriin
    try {
        throw Exception("Tämä on virhe.")
    } catch (e: Exception) {
        System.err.println(e)
    }
}
```

Yllä oleva koodi tuottaa seuraavan tulosteen:

```
Tämä on virheen viesti.
java.lang.Exception: Tämä on virhe.
```

## Syvemmälle

Kun kirjoitat tietoja standard erroriin, on tärkeää huomata, että ne eivät näy suoraan ohjelman normaalissa tulosteessa. Tämä tarkoittaa, että standard out ja standard error voivat näyttää tuloksia eri järjestyksessä, mikä voi olla hyödyllistä virheiden selvittämisessä.

Muista myös, että voit yhdistää standard errorin ja standard outin tulostukset yhteen käyttämällä `println()`-metodia. Tämä tarkoittaa sitä, että voit kirjoittaa sekä normaalin tulostuksen että virheiden viestit samalle riville.

## Katso myös

- [Kotlinin dokumentaatio standard errorin käsittelystä](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-system/index.html#err)
- [Java-koodi standard errorin kirjoittamisesta](https://www.baeldung.com/java-write-to-system-error)
- [Kotlin-kielen virallinen kotisivu](https://kotlinlang.org/)