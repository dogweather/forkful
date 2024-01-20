---
title:                "Kirjoittaminen vakiovirheelle"
html_title:           "Kotlin: Kirjoittaminen vakiovirheelle"
simple_title:         "Kirjoittaminen vakiovirheelle"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Kirjoittaminen standardivirheeseen on tapa ilmoittaa ohjelman suorituksen aikana havaituista virheistä ja poikkeustilanteista. Usein kirjoitamme virheilmoituksia standardilähtöön, mutta joskus on tarpeen käyttää erillistä virheenkorjaustilaa, jotta ohjelma voi jatkaa suoritustaan mahdollisimman sujuvasti.

## Miten:

```Kotlin
fun main() {
    val age = -5
    if (age < 0) {
        System.err.println("Ikä ei voi olla negatiivinen.")
    }
    /*
    Tämä tulostaa "Ikä ei voi olla negatiivinen." standardivirheeseen,
    koska käytämme System.err.println() -metodia.
    */
}
```

```Kotlin
try {
    //Koodi, jossa saattaa olla poikkeustilanteita
} catch (e: Exception) {
    System.err.println("Tapahtui poikkeustilanne: " + e.message)
}
/*
Tämä tulostaa virheilmoituksen standardivirheeseen,
jossa kerrotaan tapahtuneesta poikkeustilanteesta ja sen viestin sisältö.
*/
```

## Syventävä tieto:

Historiallisesti standardivirheen käyttö on ollut yleistä ohjelmoinnissa. Toisaalta modernimmat ohjelmointikielet tarjoavat myös muita tapoja käsitellä ja raportoida virheitä, kuten poikkeusten heittämisen ja käsittelyn. Käyttötapauksista riippuen standardivirheen käyttö voi olla edelleen hyödyllistä ja kätevää.

## Katso myös:

- [Exception Handling in Kotlin](https://kotlinlang.org/docs/exceptions.html)