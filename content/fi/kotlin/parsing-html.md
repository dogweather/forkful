---
title:                "Kotlin: Htm-ln parsiminen"
simple_title:         "Htm-ln parsiminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

# Miksi

Parsing HTML on tärkeä taito jokaiselle Kotlin-ohjelmoijalle, joka haluaa hyödyntää web-sivujen sisältöä omassa sovelluskehityksessään. HTML-sisällöt ovat yleinen tapa tarjota tietoa käyttäjille ja osa ohjelmien toiminnallisuutta perustuu juuri web-sivujen päivittämiseen. Tässä blogipostissa kerron, miten HTML-parsiminen voidaan toteuttaa Kotlinilla ja miten siitä voidaan saada hyötyä.

# Miten

HTML-parsiminen on prosessi, jossa haetaan tietoa web-sivuilta ja käsitellään sitä halutulla tavalla. Tämä on erityisen kätevää silloin, kun halutaan automatisoida informaation hakeminen tai hyödyntää web-sivujen sisältöä omassa sovelluskehityksessä. Alla on esimerkki, miten voit toteuttaa HTML-parsimisen Kotlinilla ja tulostaa haetut tiedot konsoliin:

```Kotlin
// Lisää tarvittavat riippuvuudet
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

// Luodaan funktio, joka hakee halutun web-sivun HTML-dokumentin
fun getHTMLDocument(url: String): Document {
    return Jsoup.connect(url).get()
}

// Haetaan ja tulostetaan HTML-dokumentista haluttu sisältö
fun parseHTML() {
    val document = getHTMLDocument("https://esimerkki.fi/")
    val title = document.title()
    println("Web-sivun otsikko: $title")
    val element = document.select("p")
    println("Ensimmäinen kappale: ${element[0].text()}")
}

// Kutsutaan funktiota
parseHTML()
```

Ohjelma tulostaa seuraavanlaisen tulosteen:

```
Web-sivun otsikko: Esimerkki.fi - HTML-parsiminen Kotlinilla
Ensimmäinen kappale: Tervetuloa lukemaan uutta blogipostaustamme! Tässä tarjoamme oppaan HTML-parsimisen toteuttamiseen Kotlinilla ja jaamme vinkkejä, miten voit hyödyntää tätä taitoa omaan käyttöösi.
```

Tässä esimerkissä haemme otsikon ja ensimmäisen kappaleen halutusta web-sivusta ja tulostamme ne konsoliin. Muokkaamalla funktiota ja käyttämällä erilaisia CSS-valitsimia, voit hakea ja käsitellä haluamiasi tietoja web-sivuilta.

# Syvällisemmin

HTML-parsiminen tulisi toteuttaa aina huolellisesti ja ottaen huomioon mahdolliset virhetilanteet, kuten puuttuvat elementit tai muutokset HTML-rakenteessa. Jsoup-kirjasto, jota käytimme esimerkissä, tarjoaa erinomaisia työkaluja näiden tilanteiden käsittelyyn, kuten `try-catch` lauseita ja metodien käyttöä varmistamaan, että haettu tieto löytyy.

Lisäksi kannattaa ottaa huomioon, että HTML-parsiminen voi olla aikaavievää, jos haettava sivusto on suuri tai jos halutaan hakea suuri määrä tietoja. Kannattaa siis aina miettiä, onko HTML-parsiminen oikea tapa toteuttaa haluamasi toiminnallisuus ja kuinka suuri vaikutus sillä on ohjelman suorituskykyyn.

# Katso myös

- [Jsoup - HTML-parsimiseen tarkoitettu Java-kirjasto](https://jsoup.org/)
- [Kotlinin viralliset verkkosivut](https://kotlinlang.org/)
- [Kotlinin dokumentaatio HTML-pars