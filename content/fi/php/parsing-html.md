---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/parsing-html.md"
---

{{< edit_this_page >}}

# HTML:n jäsentäminen PHP:llä: Nopea Opas

## Mitä & Miksi?

HTML-jäsentäminen tarkoittaa HTML-dokumentin sisällön järjestämistä jäsennellyssä muodossa, jotta koodi voi ymmärtää sen. Ohjelmoijat tekevät näin voidakseen käsitellä ja ottaa hyödyt irti web-sivun sisällöstä tekoälyn tai datan analysointiin.

## Kuinka: 

```PHP
<?php
$doc = new DOMDocument();
libxml_use_internal_errors(TRUE); // estää viestit varoituksista
$doc->loadHTML(file_get_contents('https://www.example.com'));
libxml_clear_errors();
$xpath = new DOMXPath($doc);
$nodeList = $xpath->query('//p'); // ota kaikki <p> elementit
foreach ($nodeList as $node) {
  echo $node->nodeValue, PHP_EOL; // tulosta <p> elementtien sisältö
}
?>
```
Esimerkkikoodi lataa HTML:n osoitteesta 'https://www.example.com', etsii kaikki `<p>` elementit ja tulostaa niiden sisällön.

## Syvällinen tutkimus 

HTML-jäsentäminen on ollut olemassa yhtä kauan kuin web itse. PHP:ssä sisäänrakennettu DOMDocument-luokka on yksi tapa jäsentää. Alternatiivit kuten Simple HTML DOM tai PHPQuery tarjoavat enemmän toimintoja ja joustoa. Oli työkalu mikä tahansa, keskeistä on saada HTML sisältö selkeään ja hyödynnettävään muotoon.

## Katso myös 

- PHP:n DOMDocument: [https://www.php.net/manual/en/class.domdocument.php](https://www.php.net/manual/en/class.domdocument.php)
- Simple HTML DOM: [http://simplehtmldom.sourceforge.net/](http://simplehtmldom.sourceforge.net/)
- PHPQuery: [https://code.google.com/archive/p/phpquery/](https://code.google.com/archive/p/phpquery/)