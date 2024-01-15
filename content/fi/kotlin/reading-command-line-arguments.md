---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Kotlin: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrien lukeminen on tärkeä osa ohjelmistokehitystä ja auttaa välttämään vakavia bugeja ohjelman suorituksessa. Se myös mahdollistaa käyttäjille antaa ohjelmalle erilaisia parametreja, mikä tekee siitä monipuolisemman ja käyttäjäystävällisemmän.

## Miten

Kotlinilla on helppo lukea komentoriviparametreja käyttämällä "args" muuttujaa. Se on taulukko, joka sisältää kaikki komentorivillä annetut parametrit. Seuraava esimerkki näyttää, kuinka tulostaa kaikki annetut parametrit:

```Kotlin
fun main(args: Array<String>) {
    println("Komentoriviparametrit:")
    for (arg in args) {
        println(arg)
    }
}
```

Kun ajetaan ohjelmaa komentoriviltä antaen sille esimerkiksi parametrit "one" ja "two", tuloste näyttää seuraavalta:

```
Komentoriviparametrit:
one
two
```

## Syvempi sukellus

Vaikka yllä oleva esimerkki osoittaa, kuinka helppoa komentoriviparametrien lukeminen Kotlinilla on, on tärkeää muistaa, että taulukon indeksointi alkaa aina nollasta. Tämä tarkoittaa, että ensimmäinen parametri on "args[0]", toinen "args[1]" ja niin edelleen.

Lisäksi, jos haluat käsitellä tietyntyyppisiä parametreja (esim. kokonaislukuja tai desimaalilukuja), sinun on muunnettava ne oikeaan muotoon käyttämällä Kotlinin sisäänrakennettuja metodeja (esim. toInt() tai toDouble()).

## Katso myös

 - [Kotlinin virallinen dokumentaatio komentoriviparametrien lukemisesta](https://kotlinlang.org/docs/reference/command-line.html)
 - [Kotlinin sisäänrakennetut metodiit numeroiden muuntamiseen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html#parsing-numbers)