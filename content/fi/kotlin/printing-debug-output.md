---
title:                "Tulostaminen virheenkorjaustulosteista"
html_title:           "Kotlin: Tulostaminen virheenkorjaustulosteista"
simple_title:         "Tulostaminen virheenkorjaustulosteista"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Joskus kehittäjän on tärkeää nähdä ohjelmansa sisäisiä tapahtumia ja muuttujien arvoja, jotta voi havaita ja ratkaista mahdolliset ongelmat. Tulostamalla debug-tietoa ohjelman suorituksen aikana, voidaan helpommin havaita virheitä ja parantaa koodin suorituskykyä.

## Miten tehdä niin

Debug-tietojen tulostaminen Kotlinissa on helppoa ja nopeaa. Voit käyttää standardia println-funktiota, kuten: 

```Kotlin
fun main() {
    val name = "Maija"
    val age = 25
    println("Tervetuloa, $name!")
    println("Olet $age-vuotias.")
}
```

Tämä tulostaa seuraavanlaisen viestin:

```
Tervetuloa, Maija!
Olet 25-vuotias.
```

Voit myös tulostaa monimutkaisempia tietoja käyttämällä String-litereleitä ja muuttujien nimiä. Esimerkiksi:

```Kotlin
fun main() {
    val num1 = 10
    val num2 = 5
    println("Ensimmäinen luku on $num1 ja toinen luku on ${num2 * 2}.")
}
```

Tämä tulostaa seuraavanlaisen viestin:

```
Ensimmäinen luku on 10 ja toinen luku on 10.
```

## Syvällisempi tarkastelu

Voi olla hyödyllistä tietää, että voit myös tulostaa debug-tietoja suoraan debug-luokan avulla. Tämä luokka tarjoaa erilaisia metodeja tietojen tarkasteluun ja formaattaukseen. Esimerkiksi voit käyttää "debug" -funktiota, kuten:

```Kotlin
fun main() {
    val name = "Markku"
    val age = 30
    val height = 175.5
    val weight = 80.6
    debug("Nimi: $name, Ikä: $age, Pituus: $height cm, Paino: $weight kg")
}
```

Tulostus näyttäisi tältä:

```
Debug: Nimi: Markku, Ikä: 30, Pituus: 175.5 cm, Paino: 80.6 kg
```

Voit myös käyttää muita debug-luokan metodeja, kuten "debugToString", "debugColors" ja "debugIndent". Lisätietoja löydät [Kotlinin virallisesta dokumentaatiosta](https://kotlinlang.org/docs/reference/using-gradle.html).

## Katso myös

- [Kotlinin viralliset verkkosivut](https://kotlinlang.org/)
- [Kotlinin dokumentaatio](https://kotlinlang.org/docs/home.html)
- [Ohjelmoinnin perusteet Kotlinilla](https://developer-tech.com/learn-kotlin-beginners/)