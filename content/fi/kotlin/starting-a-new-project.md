---
title:    "Kotlin: Aloittamassa uutta projektia"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi

Aloittamalla uuden projektin voi saada uusia haasteita ja päästä oppimaan uusia taitoja.

## Kuinka aloittaa

Aloitetaan uusi projekti käyttäen Kotlin-ohjelmointikieltä. Ensimmäisenä on tärkeää luoda uusi Kotlin-projekti. Tätä varten avataan ensin suosittu integroitu kehitysympäristö, kuten IntelliJ IDEA tai Android Studio.

Sitten luodaan uusi projektikansio ja lisätään siihen build.gradle-tiedosto. Tämän jälkeen määritellään projektille tarvittavat riippuvuudet. Riippuvuuksia ovat esimerkiksi Kotlin-käännöstyökalut ja tarvittavat kirjastot.

```Kotlin
dependencies {
    compile "org.jetbrains.kotlin:kotlin-stdlib-jdk8"
    testCompile "junit:junit:4.12"
}
```

Seuraavaksi luodaan ensimmäinen Kotlin-tiedosto ja aloitetaan koodaaminen. Esimerkiksi voidaan luoda funktio, joka laskee kahden luvun summan ja tulostaa sen konsoliin.

```Kotlin
fun sum(a: Int, b: Int): Int {
    return a + b
}

fun main(args: Array<String>) {
    println("Summa: " + sum(5, 3))
}
```

Tämän jälkeen suoritetaan koodi ja tulostetaan summa konsoliin.

```
> Summa: 8
```

## Syväsukellus

Uuden projektin aloittaminen ei ole koskaan helppoa, mutta Kotlin-ohjelmointikieli tekee siitä hieman helpompaa. Kotlin tarjoaa selkeän ja yksinkertaisen syntaksin, joten sitä on helppo oppia ja käyttää. Lisäksi se on yhteensopiva Java-kielen kanssa, joten se on hyvä valinta esimerkiksi Android-sovelluksien kehittämiseen.

Projektin aloittamisen yhteydessä on myös tärkeää suunnitella tarkasti, millaisen projektin haluaa luoda. Onko kyseessä esimerkiksi mobiilisovellus, web-sovellus vai jokin muu? Tämän lisäksi on hyvä huomioida myös projektin tavoitteet sekä käytettävät teknologiat ja kirjastot.

Nyt kun uusi projekti on luotu, aloita rohkeasti koodaaminen ja opi uutta matkan varrella!

---

## Katso myös

- [Kotlin - viralliset verkkosivut](https://kotlinlang.org/)
- [Kotlin in Action - ohjelmointikirja](https://www.manning.com/books/kotlin-in-action)
- [IntelliJ IDEA - integroitu kehitysympäristö Kotlinille](https://www.jetbrains.com/idea/)
- [Android Studio - integroitu kehitysympäristö Android-sovellusten kehitykseen](https://developer.android.com/studio/)