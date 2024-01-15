---
title:                "HTML:n jäsentäminen"
html_title:           "Kotlin: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Monet moderneista nettisivuista ovat tehty käyttämään HTML-kieltä, joka on tietokonerajapinta sivujen muotoiluun ja sisällön esittämiseen. Siksi on tärkeää, että ohjelmoijina osaamme parsia HTML:ää ymmärtääksemme sivustojen rakennetta ja kerätäksemme sieltä tarvitsemamme tiedot.

## Kuinka tehdä

HTML:n parsiminen voidaan tehdä monella eri tavalla käyttäen erilaisia työkaluja ja kirjastoja. Yksi suosituimmista vaihtoehdoista on käyttää Kotlinia, joka on nykyaikainen ja suosittu ohjelmointikieli. Se yhdistää puhtaasti funktionaalisen ja olio-orientoituneen ohjelmoinnin parhaita puolia.

Yksi tapa parsia HTML:ää Kotlinilla on käyttämällä JSoup-kirjastoa, joka on Java-kirjasto, mutta sitä voidaan myös käyttää Kotlin-projekteissa. Alla on esimerkki, miten voit ladata ja parsia nettisivun HTML:ää käyttäen Kotlinia:

```Kotlin
// Lisätään JSoup-kirjasto
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

// Luodaan funktio nimeltä `parseHTML`, joka saa argumenttina URL-osoitteen
fun parseHTML(url: String) {
    // Ladataan HTML dokumentti annetusta URL-osoitteesta
    val document: Document = Jsoup.connect(url).get()

    // Käytetään CSS-selektoreita löytääksemme haluamamme elementit sivulta
    val title: String = document.select("h1").text()
    val paragraphs: List<String> = document.select("p").eachText()

    // Tulostetaan parsitut elementit
    println("Sivun otsikko: $title")
    println("Kappaleet: $paragraphs")
}

// Kutsutaan funktiota antamalla sille URL-osoite
parseHTML("https://example.com")
```

Tulostuksena saat HTML-sivun otsikon ja listaun kappaleista:

```
Sivun otsikko: Esimerkki Sivu
Kappaleet: [Tämä on ensimmäinen kappale, Tämä on toinen kappale, Ja tämä on kolmas kappale]
```

## Syvällisempi tarkastelu

HTML:n parsiminen ei rajoitu pelkästään netissä olevien sivujen muotoilun analysointiin, vaan sitä voidaan myös käyttää tiedonkeruuseen ja datan tallentamiseen tietokantaan. Esimerkiksi voit parsia uutissivuston HTML:ää ja tallentaa sieltä löydetyt uutisotsikot ja linkit omaan tietokantaasi, josta voit myöhemmin hakea haluamiasi uutisia.

Parsittaessa HTML:ää on tärkeää ymmärtää sivuston rakennetta ja miten eri elementtejä voidaan valita käyttäen CSS-selektoreita. Myös tiedon käsittely kannattaa tehdä tehokkaasti, jotta parsiminen ei hidasta ohjelman suorituskykyä.

## Katso myös

- [JSoup kirjaston kotisivu](https://jsoup.org/)
- [Kotlin viralliset dokumentaatiot](https://kotlinlang.org/docs/home.html)
- [HTML ja CSS opetusmateriaali](https://www.w3schools.com/htmL)