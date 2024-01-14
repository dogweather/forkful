---
title:                "PHP: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Kirjoittamalla testejä varmistetaan, että koodi toimii tarkoitetulla tavalla ja että mahdolliset virheet voidaan löytää ja korjata jo ennen kuin koodi on käyttöönotossa. Tämä säästää aikaa ja vaivaa ja varmistaa paremman lopputuloksen.

## Miten
```PHP
<?php
function laskeKolmionPintaAla($kanta, $korkeus) { 
  $pintaAla = ($kanta * $korkeus) / 2; 
  return $pintaAla;
}
?>

Testaa funktiota laskemalla kolmion pinta-ala annetulla kannalla ja korkeudella:

```PHP
<?php
$kanta = 5;
$korkeus = 10;

$pintaAla = laskeKolmionPintaAla($kanta, $korkeus);

echo "Kolmion pinta-ala on: " . $pintaAla;
?>

Tulos:
Kolmion pinta-ala on: 25
```

## Syvällinen sukellus
Testien kirjoittaminen auttaa myös koodin skaalautuvuudessa ja ylläpidettävyydessä. Hyvin kirjoitetut testit toimivat myös dokumentaationa koodille ja auttavat uusia kehittäjiä ymmärtämään koodin toimintaa ja rakennetta.

## Katso myös
- [SimpleTest:n opas testien kirjoittamiseen PHP:lla](http://www.simpletest.org/en/start-testing.html)
- [PHPUnit:n opas testien kirjoittamiseen PHP:lla](https://phpunit.de/manual/current/en/writing-tests-for-phpunit.html)
- [PHP:n sisäänrakennetut testausmahdollisuudet](https://www.php.net/manual/en/function.assert.php)