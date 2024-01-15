---
title:                "HTML:n jäsentäminen"
html_title:           "PHP: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

HTML-analyysi on tärkeä taito PHP-ohjelmoijille, koska se mahdollistaa tietojen keräämisen verkosta ja niiden käsittelyn omassa koodissa. Tämä voi säästää aikaa ja vaivaa manuaalisesta tiedon keräämisestä ja parantaa ohjelman tehokkuutta.

## Kuinka tehdä

Analysointikoodin suorittaminen:

```PHP
$html = '<h1>Otsikko</h1><p>Tämä on esimerkkiparagraafi<h2>Toinen otsikko</h2>';
$dom = new DOMDocument();
$dom->loadHTML($html);
$elements = $dom->getElementsByTagName('h1');

foreach ($elements as $element) {
    echo $element->nodeValue; // Tulostaa "Otsikko"
}

$paragraphs = $dom->getElementsByTagName('p');
foreach ($paragraphs as $paragraph) {
    echo $paragraph->nodeValue; // Tulostaa "Tämä on esimerkkiparagraafi"
}

// Voit myös hakea elementtejä id-tunnisteella:
$element = $dom->getElementById('id-tunniste');
echo $element->nodeValue; // Tulostaa kyseisen elementin sisällön
```

Voit myös hakea tietoja eri tyyppisistä HTML-elementeistä, kuten linkeistä, kuvista ja lomakkeista. Kun olet hakenut tiedot haluamistasi elementeistä, voit käsitellä niitä haluamallasi tavalla, esimerkiksi tallentaa ne tietokantaan tai näyttää ne käyttäjälle.

## Syvempi sukellus

HTML-analyysin käyttö on tärkeä taito PHP-ohjelmoijille, sillä se mahdollistaa monipuolisen tiedon keräämisen ja käsittelyn. Analysoimalla HTML:ää voit myös parantaa verkkosivujen suorituskykyä, esimerkiksi optimoimalla CSS- ja JavaScript-tiedostoja tai poistamalla tarpeettoman koodin.

On tärkeää muistaa, että HTML-tietojen analysointi ei ole täysin virheetöntä, sillä sivustojen rakenne ja koodin laatu vaihtelevat. Siksi on hyvä testata analysointikoodia erilaisilla sivustoilla ja ottaa huomioon mahdolliset virheelliset tulokset.

## Katso myös

- [PHP:n viralliset dokumentaatiot HTML-analyysista](https://www.php.net/manual/en/domdocument.loadhtml.php)
- [W3Schools HTML-analyysin harjoituksia](https://www.w3schools.com/php/php_ref_dom.asp)
- [Stack Overflowin ohjeita HTML-analyysin käyttöön](https://stackoverflow.com/questions/tagged/html-parsing?sort=votes)