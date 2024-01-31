---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Isot alkukirjaimet tekevät merkkijonosta helpommin luettavan ja tekevät siitä näyttävämmän – kuten otsikoissa tai nimissä. Ohjelmoijat käyttävät tätä parantaakseen käyttöliittymän ulkoasua ja käyttäjäkokemusta.

## How to: (Näin toimit)
PHP:ssä merkkijonon muuttaminen kokonaan isoilla kirjaimilla voidaan tehdä `strtoupper()`-funktiolla ja alkukirjaimen isontaminen `ucfirst()`-funktiolla. Esimerkki:

```php
<?php
$small_text = "tämä on testi";
$capitalized_text = ucfirst($small_text);
$uppercased_text = strtoupper($small_text);

echo $capitalized_text; // Tulostuu: Tämä on testi
echo "\n";
echo $uppercased_text;  // Tulostuu: TÄMÄ ON TESTI
?>
```

## Deep Dive (Syväsukellus)
Merkin isontaminen on yleinen käytäntö monissa ohjelmointikielissä, ja PHP ei tee tästä poikkeusta. Historiallisesti, kun tietokoneet olivat nuorempia ja käyttöliittymät pelkistetympiä, isot kirjaimet olivat hyvin tärkeässä roolissa painottaessaan tärkeää tietoa. PHP:ssä `strtoupper()` ja `ucfirst()` suorittavat tämän tehtävän muuttamalla merkkijonon UTF-8-yhteensopivaksi, mikä varmistaa, että myös suurin osa erikoismerkeistä käsitellään oikein. Vaihtoehtoisesti `mb_strtoupper()`-funktio tarjoaa tuen monille eri merkistöille, mikä on tärkeää, kun työskentelet monikielisissä sovelluksissa.

## See Also (Katso Myös)
- PHP:n virallinen dokumentaatio `strtoupper()`: https://www.php.net/manual/en/function.strtoupper.php
- PHP:n virallinen dokumentaatio `ucfirst()`: https://www.php.net/manual/en/function.ucfirst.php
- PHP:n virallinen dokumentaatio `mb_strtoupper()` (monikielisille merkkijonoille): https://www.php.net/manual/en/function.mb-strtoupper.php
