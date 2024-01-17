---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "PHP: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Kirjoittaminen standardivirheeseen (standard error) tarkoittaa virheiden tai ilmoitusten tallentamista koodin suorituksen aikana. Tämä auttaa kehittäjiä korjaamaan sovelluksiin liittyviä ongelmia ja kehittämään nämä sovellukset paremmaksi. 

Kehittäjät käyttävät kirjoittamista standardivirheeseen saadakseen lisätietoa sovellustensa toiminnasta, kuten virheitä tai mahdollisia suorituskykyongelmia.

## Miten tehdä:
```
<?php
error_log("Tarkista tämä virhe!"); // Lisää teksti "Tarkista tämä virhe!" standardivirheeseen
echo "Jokin teksti"; // Tulostaa "Jokin teksti" konsoliin
?>
```
Tulostus:
```
Tarkista tämä virhe!
Jokin teksti
```

## Syvemmälle:
Kirjoittaminen standardivirheeseen on käytäntö joka on käytössä monissa ohjelmointikielissä, sillä se auttaa kehittäjiä löytämään ja korjaamaan virheitä nopeammin. Tämä auttaa myös kehittäjiä luomaan vakaampia ja luotettavampia sovelluksia. 

Pääasiallinen vaihtoehto kirjoittamiselle standardivirheeseen on tallentaa virheet ja ilmoitukset lokitiedostoihin, mutta tämä voi olla hankalampaa löytää ja tarkastella. Kirjoittaminen standardivirheeseen on nopeampi ja helpompi tapa saada lisätietoa sovelluksen suorituksesta.

PHP:ssa, virheitä voi tallentaa käyttämällä funktiona error_log() tai käyttämällä lähteen koodia, kuten esimerkissä nähdään. Virheidentallentaminen voidaan myös kytkeä pois päältä asetuksista, mikäli sitä ei haluta käyttää.

## Katso myös:
- PHP: virheitä käsittelevä dokumentaatio: https://www.php.net/manual/en/book.errorfunc.php
- PHP: error_log-funktio: https://www.php.net/manual/en/function.error-log.php