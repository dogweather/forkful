---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:33:06.941973-07:00
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
HTML:n jäsentäminen tarkoittaa HTML-dokumentin rakenteen muuttamista sellaiseen muotoon, jonka ohjelmakoodi voi ymmärtää ja käsitellä. Kehittäjät tekevät tätä, jotta voivat lukea, muokata tai saada tietoa sivun sisällöstä ohjelmallisesti.

## How to:
```PHP
<?php
// Yksinkertainen esimerkki DOMDocumentin käytöstä HTML:n jäsentämiseen
$dom = new DOMDocument();
$html = '<!DOCTYPE html><html><body><h1>Hei Suomi!</h1></body></html>';
@$dom->loadHTML($html);
$h1 = $dom->getElementsByTagName('h1')->item(0);
echo $h1->textContent;
// Tulostaa: Hei Suomi!
?>
```

## Deep Dive
HTML:n jäsentäminen PHP:ssä on ollut mahdollista jo kauan, klassisesti `DOMDocument`-luokan avulla. PHP 5 toi mukanaan paremmat työkalut ja PHP 7+ on parantanut suorituskykyä. Vaikka `DOMDocument` on suosittu, on olemassa myös muita kirjastoja, kuten `SimpleXML` ja `phpQuery`. 

Jäsentämisen yksityiskohtia miettiessä tärkeää on ymmärtää, että HTML on puurakenteinen: elementit sisältävät toisiaan. Jäsentäjän täytyy siis tukea tätä rakennetta, ymmärtää tagien väliset suhteet ja osata käsitellä myös virheellistä HTML-koodia, jota internetissä on paljon. `DOMDocument`-luokka osaa korjata puutteita, kuten puuttuvia lopetustageja, mikä on kätevää mutta voi aiheuttaa odottamattomia tuloksia.

Kun valitsee työkalua HTML:n jäsentämiseen, kannattaa miettiä tarpeita: suorituskykyä, helppokäyttöisyyttä, virheenkestävyyttä ja yhteensopivuutta. Suurille dokumenteille tai raskaalle liikenteelle suunnitellut ratkaisut voivat olla erilaisia kuin kevyisiin skripteihin soveltuvat työkalut.

## See Also
- Official PHP Documentation on DOM: [php.net/manual/en/book.dom.php](https://www.php.net/manual/en/book.dom.php)
- SimpleXML: [php.net/manual/en/book.simplexml.php](https://www.php.net/manual/en/book.simplexml.php)
- phpQuery GitHub repository: [github.com/phpquery/phpquery](https://github.com/phpquery/phpquery)
