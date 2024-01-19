---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen muuttaminen pienaakkosiksi tarkoittaa merkkijonojen muuttamista siten, että kaikki suuraakkoset korvataan vastaavilla pienaakkosilla. Tämän avulla voimme tehdä tietojen vertailusta yksinkertaisempaa ja vähemmän virhealtista. 

## Näin se toimii:

Sisäänrakennetun `strtolower()` funktion avulla PHP:ssä voidaan muuttaa merkkijonot pienaakkosiksi. 
Katso esimerkki alla:

```PHP
<?php
$myStr = "PÄIVÄÄ SUOMI";
echo strtolower($myStr);
?>
```
Tässä tapauksessa tuloste olisi "päivää suomi".

## Syvä sukellus:

Kun `strtolower()` esiteltiin alunperin PHP 3:ssa, sitä on käytetty laajasti siistimään ja normalisoimaan tietoja. Se on edelleen tärkeä osa PHP:n merkkijonon käsittelyvälineitä. 

Vaihtoehtoja on, kuten `mb_strtolower()`, joka hoitaa monikieliset merkkijonot paremmin, mutta `strtolower()` on yleisimmin käytetty tapa PHP:ssa. 

Nämä funktiot toimivat skannaamalla läpi merkkijonon ja korvaamalla jokainen suuraakkonen vastaavalla pienaakkosella käyttäen ASCII-arvotaulukkoa. 

## Katso myös:

Lisätietoa saadaksesi, tutustu seuraaviin lähteisiin:
- [PHP:n virallinen ohjekirja strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [PHP:n virallinen ohjekirja mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)