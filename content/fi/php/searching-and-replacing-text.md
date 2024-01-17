---
title:                "Tekstin hakeminen ja korvaaminen"
html_title:           "PHP: Tekstin hakeminen ja korvaaminen"
simple_title:         "Tekstin hakeminen ja korvaaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin etsiminen ja korvaaminen on yleinen ohjelmoinnin käsite, joka tarkoittaa tiettyjen merkkijonojen tai osien etsimistä ja niiden korvaamista toisilla. Tämä on tärkeä työkalu, jota ohjelmoijat käyttävät muun muassa tehokkaamman koodin luomiseen ja virheiden korjaamiseen.

## Miten:

Esimerkiksi, jos haluat etsiä tietyn sanan tai lauseen ja korvata sen toisella, voit käyttää PHP:n ```str_replace()``` funktiota. Tässä on yksinkertainen esimerkki koodista ja sen tulosteesta:
```
$input = "Tervetuloa kurssille!";
$output = str_replace("Tervetuloa", "Hei", $input);
echo $output; // tulostaa "Hei kurssille!"
```
Voit myös käyttää säännöllisiä lausekkeita tekstien etsimiseen ja korvaamiseen. Esimerkiksi, jos haluat korvata kaikki välimerkit tyhjillä, voit käyttää ```preg_replace()``` funktiota:
```
$string = "Tämä on esimerkki: ääkkösiä!";
$output = preg_replace("/[^\p{L}]/u", '', $string);
echo $output; // tulostaa "TämäonEsimerkiääkkösiä"
```

## Syvemmälle:

Tekstin etsiminen ja korvaaminen on ollut osa ohjelmointia jo vuosikymmenten ajan ja sitä on käytetty erilaisiin tarkoituksiin, kuten datan siirtoon ja käännöstyöhön. Lisäksi on olemassa muita vaihtoehtoisia tapoja toteuttaa tämä toiminto, kuten awk, sed ja Perl.

Tekstin etsiminen ja korvaaminen on myös mahdollista tehdä käyttäen PHP:n omia string funktioita, kuten ```str_replace()``` ja ```substr_replace()```. Näiden toimintojen ero on lähinnä siinä, että säännöllisiä lausekkeita voi käyttää vain ```preg_replace()``` funktion kanssa.

## Katso myös:

- [PHP:n virallinen dokumentaatio tekstien etsimisestä ja korvaamisesta](https://www.php.net/manual/en/function.preg-replace.php)
- [Selitys regular expression säännöistä](http://www.rexegg.com/regex-quickstart.html)