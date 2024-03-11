---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:50.524019-07:00
description: "HTML:n j\xE4sent\xE4minen PHP:lla tarkoittaa tiettyjen tietojen poimimista\
  \ HTML-dokumenteista. Ohjelmoijat suorittavat t\xE4m\xE4n teht\xE4v\xE4n automatisoidakseen\u2026"
lastmod: '2024-03-11T00:14:30.605323-06:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4minen PHP:lla tarkoittaa tiettyjen tietojen poimimista\
  \ HTML-dokumenteista. Ohjelmoijat suorittavat t\xE4m\xE4n teht\xE4v\xE4n automatisoidakseen\u2026"
title: "HTML:n j\xE4sennys"
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTML:n jäsentäminen PHP:lla tarkoittaa tiettyjen tietojen poimimista HTML-dokumenteista. Ohjelmoijat suorittavat tämän tehtävän automatisoidakseen tietojen keruun, verkkosivujen kaapimisen tai integroidakseen sisältöä eri verkkosivuilta sovelluksiinsa, parantaen toiminnallisuutta ilman manuaalista sekaantumista.

## Kuinka:
HTML:n jäsentämiseen PHP-ohjelmoijat voivat hyödyntää sisäänrakennettuja funktioita tai nojata vankkoihin kirjastoihin, kuten Simple HTML DOM Parser. Tässä tutustumme esimerkkeihin käyttäen sekä PHP:n `DOMDocument` että Simple HTML DOM Parseria.

### Käyttäen `DOMDocument`:
PHP:n `DOMDocument`-luokka on osa sen DOM-laajennosta, jonka avulla voi jäsentää ja manipuloida HTML- ja XML-dokumentteja. Tässä on nopea esimerkki siitä, miten `DOMDocument`-luokkaa voidaan käyttää kaikkien kuvien löytämiseen HTML-dokumentista:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Esimerkkisivu</title>
</head>
<body>
    <img src="image1.jpg" alt="Kuva 1">
    <img src="image2.jpg" alt="Kuva 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$kuvat = $doc->getElementsByTagName('img');

foreach ($kuvat as $kuva) {
    echo $kuva->getAttribute('src') . "\n";
}
```

Esimerkkituloste:
```
image1.jpg
image2.jpg
```

### Käyttäen Simple HTML DOM Parseria:
Monimutkaisempiin tehtäviin tai helpompaan syntaksiin saatat mieluummin käyttää kolmannen osapuolen kirjastoa. Simple HTML DOM Parser on suosittu valinta, ja se tarjoaa jQueryn kaltaisen rajapinnan navigoimiseen ja HTML-rakenteiden manipulointiin. Näin sitä käytetään:

Ensiksi, asenna kirjasto käyttäen Composeria:
```
composer require simple-html-dom/simple-html-dom
```

Sen jälkeen, manipuloi HTML:ää esimerkiksi löytääksesi kaikki linkit:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$asiakas = new HtmlWeb();
$html = $asiakas->load('http://www.example.com');

foreach($html->find('a') as $elementti) {
    echo $elementti->href . "\n";
}
```

Tämä koodinpätkä hakee 'http://www.example.com' -sivun HTML-sisällön, jäsentää sen ja tulostaa kaikki hyperlinkit. Muista korvata `'http://www.example.com'` todellisella URL-osoitteella, jonka haluat jäsentää.

Käyttämällä näitä menetelmiä PHP-kehittäjät voivat tehokkaasti jäsentää HTML-sisältöä, räätälöidä tietojen keruun tarpeisiinsa tai saumattomasti integroida ulkoista verkkosisältöä projekteihinsa.
