---
title:                "PHP: HTML:n jäsentäminen."
simple_title:         "HTML:n jäsentäminen."
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/parsing-html.md"
---

{{< edit_this_page >}}

# Miksi HTML:n jäsentäminen on tärkeää

HTML-jäsennys on tärkeä taito jokaiselle PHP-ohjelmoijalle. Se mahdollistaa tiedon hakemisen ja muokkaamisen HTML-koodista, mikä on usein tarpeen web-sivujen kehittämisessä. Lisäksi se auttaa automatisoimaan rutiinitehtäviä ja säästämään aikaa.

## Miten tehdä HTML:n jäsentäminen PHP:lla

PHP:lla on kätevät työkalut HTML-dokumenttien jäsentämiseen ja manipuloimiseen. Alla on esimerkkejä, joissa käytetään PHP:n "```simple_html_dom```" -kirjastoa.

### Esimerkki 1: Tiedon hakeminen HTML-dokumentista

Seremoniaorkesterin sivuilla on liuta jäsenten nimiä ja soittimia. Voimme hakea nämä tiedot "```find```" -metodilla.

```PHP
<?php
include('simple_html_dom.php');
$html = file_get_html('http://www.seremo.fi/?s=soittaja');
foreach($html->find('.soittaja') as $element) {
    $nimi = $element->find('.nakyviin');
    $soitin = $element->find('.nakyviin img');
    echo $nimi . ' soittaa ' . $soitin;
}
?>
```

Tulos:

>Leevi Malinen soittaa kitara<br>
>Kirsi Toivonsalmi soittaa viulu<br>
>Tino Siren soittaa rummut

### Esimerkki 2: Tiedon muokkaaminen ja tallentaminen

Voimme myös muokata HTML-dokumentin tietoja ja tallentaa muutokset takaisin dokumenttiin. Alla olevassa esimerkissä muutamme otsikkotagin arvon ja tallennamme muutokset takaisin HTML-dokumenttiin.

```PHP
<?php
include('simple_html_dom.php');
$html = file_get_html('https://www.php.net/');

// Muutetaan otsikkotagin arvo
$html->find('h1', 0)->innertext = 'Tervetuloa PHP-ohjelmointimaailmaan';

// Tallennetaan muutokset takaisin tiedostoon
file_put_contents('php.html', $html);
?>
```

### Tulos:

Voit tarkistaa "php.html" -tiedostosta, että otsikkotagin arvo on muuttunut.

## Syvempi sukellus HTML:n jäsentämiseen

HTML:n jäsentämiseen on käytettävissä myös muita työkaluja, kuten "```DOMDocument```" ja "```XPath```". Nämä ovat hyödyllisiä, kun halutaan tarkemman ja monimutkaisemman jäsentämisen, kuten tietojen hakemisen tietystä elementistä ja sen alielementeistä.

## Katso myös

- [simple_html_dom-kirjaston dokumentaatio](https://simplehtmldom.sourceforge.io/)
- [PHP:n virallinen dokumentaatio HTML:n jäsentämisestä](https://www.php.net/manual/en/domdocument.loadhtml.php)
- [XPath-opas](https://www.w3schools.com/xml/xpath_intro.asp)