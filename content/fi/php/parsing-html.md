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

## Mitä & Miksi?

HTML-analysointi tarkoittaa HTML-koodin käsittelemistä ja siihen liittyvien tietojen erottamista. Ohjelmoijat tekevät tätä esimerkiksi verkkosivujen sisällön keräämiseen tai sivustojen suunnittelutyökalujen luomiseen.

## Miten:

```PHP
// Esimerkki HTML-koodista:
$html = '<h1>Tervetuloa maailmaan</h1>';
// Käytä PHP-funktiota simplexml_load_string() erottamaan koodin avainelementit (tag) ja tekstit (text):
$xml = simplexml_load_string($html);

// Tulostaa avainelementin (tag):
echo $xml->getName(); // output: h1

// Tulostaa tekstin:
echo $xml; // output: Tervetuloa maailmaan
```

## Syventyminen:

HTML-analysointi alkoi jo varhain, kun ensimmäiset verkkosivut alkoivat ilmestyä internetiin 1990-luvulla. Nykyään on olemassa myös muita tapoja käsitellä HTML-koodia, kuten DOM-ext tai HTML-parsereita kuten Simple HTML DOM. Käyttövaihtoehtoihin kuuluu myös käyttää JavaScript-kirjastoa nimeltä jQuery, joka tarjoaa yksinkertaisen ja helpon tavan analysoida HTML-sivuja.

## Katso myös:

- [W3Schools: PHP SimpleXML -funktio](https://www.w3schools.com/PHP/func_simplexml_load_string.asp)
- [W3Schools: HTML DOM -esitehtävä](https://www.w3schools.com/js/js_htmldom.asp)
- [jQuery:n virallinen verkkosivusto](https://jquery.com/)