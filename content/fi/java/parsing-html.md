---
title:                "Java: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi: Miksi ohjelmoijan kannattaa ottaa mukaan HTML-analyysi?

HTML-analyysi on tärkeä osa monia Java-projekteja, joissa käsitellään verkkosivuja ja niiden sisältöä. HTML-analyysi mahdollistaa verkkosivun rakenteen ja tiedon erottamisen ja käsittelyn, mikä voi olla oleellista esimerkiksi web scrapingin tai tiedon keräämisen yhteydessä. Lisäksi se voi auttaa virheiden ja ongelmien havaitsemisessa ja korjaamisessa.

## Kuinka: HTML-analyysin koodin kirjoittaminen

```Java
// Tuodaan tarvittavat kirjastot
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// Määritellään URL-osoite
String url = "https://www.example.com/";

// Luodaan Document-olio ja haetaan sivun HTML-sisältö
Document doc = Jsoup.connect(url).get();

// Etsitään haluttu elementti tai sisältö sivun rakenteesta
Element title = doc.select("h1").first();

// Tulostetaan elementin teksti
System.out.println("Otsikko: " + title.text());

// Etsitään useampi elementti ja tulostetaan ne
Elements paragraphs = doc.select("p");
for (Element p : paragraphs) {
    System.out.println("Kappale: " + p.text());
}
```

Output:

```
Otsikko: Tervetuloa esimerkkisivulle!
Kappale: Tämä on ensimmäinen kappale.
Kappale: Tämä on toinen kappale.
```

## Syvällisemmin: HTML-analyysin tarkempi tutkimus

HTML-analyysilla on mahdollista tutkia sekä sivun rakennetta että sen sisältöä. Sivun rakenteen analysointiin käytetään esimerkiksi CSS-selektoreita, joiden avulla voidaan hakea halutut elementit sivun DOM-puusta. Sisällön analysoinnissa hyödynnetään erilaisia JSoup-kirjaston tarjoamia metodeja, kuten `text()`, `html()`, `attr()`, `hasClass()`, jne.

HTML-analyysin avulla voi myös suorittaa erilaisia manipulaatioita sivun sisältöön, kuten elementtien poistamista tai muokkaamista. Se on myös hyödyllistä silloin, kun halutaan hakea tietoa usealta eri sivulta ja käsitellä sitä automaattisesti.

## Katso myös

- [JSoup - Java HTML-analyysin kirjasto](https://jsoup.org/)
- [CSS-selektorit](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors)
- [Web scraping - Wikipedia (englanniksi)](https://en.wikipedia.org/wiki/Web_scraping)