---
title:                "Kotlin: Tarkista, onko kansio olemassa"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
Joku, joka käyttää Kotlinia ohjelmointikieleenään, saattaa olla kiinnostunut tarkistamaan, onko kansio olemassa. Tämä voi olla hyödyllistä esimerkiksi tiedostonhallinnassa tai erilaisten tiedostoihin liittyvien toimintojen toteuttamisessa.

## Miten tehdä
Kotlinissa on erilaisia tapoja tarkistaa, onko kansio olemassa. Yksi tapa on käyttää File-luokan metodia nimeltä "exists", joka palauttaa boolean-arvon riippuen siitä, onko tiedostopolku olemassa vai ei. Alla on esimerkki koodista ja sen tulosteesta:

```Kotlin
val directory = File("polku/kansioon")
if(directory.exists()) {
    println("Kansio on olemassa.")
} else {
    println("Kansiota ei löytynyt.")
}
```

Tämä koodi ensin luo uuden File-olion, joka viittaa kyseiseen kansioon. Sitten se tarkistaa, onko kansio olemassa ja tulostaa sen perusteella joko "Kansio on olemassa." tai "Kansiota ei löytynyt.".

Toinen tapa tarkistaa kansio on käyttää Path-luokan metodia "toFile", joka muuttaa polun tiedostosta File-olioksi. Alla on esimerkki tästä:

```Kotlin
val path = Paths.get("polku/kansioon")
val directory = path.toFile()
if(directory.exists()) {
    println("Kansio on olemassa.")
} else {
    println("Kansiota ei löytynyt.")
}
```

Tämä koodi ensin käyttää Paths-luokkaa muuttamaan polun tiedostosta Path-olioksi ja sitten Path-luokan metodia "toFile" muuttamaan sen File-olioksi. Sitten se tarkistaa, onko kansio olemassa ja tulostaa sen perusteella joko "Kansio on olemassa." tai "Kansiota ei löytynyt.".

On tärkeää huomata, että nämä kaksi tapaa voivat olla hyödyllisiä eri tilanteissa, kuten esimerkiksi jos tarkistat kansioita useissa eri tiedostopoluissa.

## Syvällinen sukellus
Molemmissa esimerkeissä käytettiin File-luokkaa ja sen exists-metodia tarkistamaan kansio. Tämä metodi käyttää alustan oman tiedostojärjestelmän rajapintaa tarkistaakseen tiedostopolun olemassaolon. Toisin sanoen, se tarkistaa alustalle tietämättömänä, onko tiedostopolku olemassa vai ei. Tämän vuoksi sen käyttäminen voi olla hyvä tapa tarkistaa kansioita, jos haluat varmistaa, että koodisi toimii alustasta riippumatta.

## Katso myös
- [File-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Path-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.file.-path/)