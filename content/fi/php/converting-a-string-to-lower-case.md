---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "PHP: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonon muuttaminen pieniksi kirjaimiksi on yleinen ohjelmoinnin tehtävä, jossa muutetaan merkkijonon kaikki kirjaimet pieniksi. Tätä tehdään yleensä helpottamaan merkkijonon vertailua ja käsittelemistä. Esimerkiksi "Tämä on TESTI" ja "tämä on testi" ovat eri merkkijonoja, vaikka ne olisivatkin sisällöltään samat. Pieniksi kirjaimiksi muuttamisen avulla molemmat merkkijonot olisivat identtisiä ja vertailu olisi helpompaa.

## Näin teet sen:
```php
$str = "Tämä on TESTI";
echo strtolower($str); // tulostaa "tämä on testi"
```

Pieniksi kirjaimiksi muuttaminen on helppoa PHP:ssä käyttämällä strtolower()-funktiota, jolle annetaan parametrina käsiteltävä merkkijono. Funktio palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat pieniä.

## Syvempi sukellus:
Historiallisessa kontekstissa merkkijonon muuttaminen pieniksi kirjaimiksi on tärkeää, koska vanhemmat järjestelmät eivät välttämättä tue isoja ja pieniä kirjaimia samalla tavalla. Tämä voi aiheuttaa ongelmia esimerkiksi tietokantojen kanssa, joissa merkkijonot tallennetaan tietyillä säännöillä. Siksi merkkijonon muuttaminen pieniksi kirjaimiksi voi olla välttämätöntä, jotta järjestelmät toimivat oikein ja tiedot tallentuvat oikein.

Vaihtoehtoja merkkijonon muuttamiseen pieniksi kirjaimiksi on erilaisia, ja onkin tärkeää valita oikea tapa kunkin tilanteen mukaan. Esimerkiksi mb_strtolower()-funktio on samanlainen kuin strtolower(), mutta se toimii myös monikielisissä ympäristöissä. Myös muut koodausstandardit ja työkalut voivat tarjota erilaisia tapoja käsitellä merkkijonoja.

Teknisiä yksityiskohtia merkkijonon muuttamisesta pieniksi kirjaimiksi voi olla mielenkiintoista tutkia, mutta yleensä tuntemus itse toiminnasta ei ole välttämätöntä. Riittää, kun tietää, miten käyttää valmiita toimintoja ja valita oikea vaihtoehto käyttötilanteen mukaan.

## Katso myös:
- [PHP:tä muiden ohjelmointikielten näkökulmasta](https://www.toptal.com/php/your-php-is-not-my-php)
- [Perusohjeita merkkijonojen käsittelyyn PHP:ssä](https://www.sitepoint.com/processing-strings-using-php/)
- [mb_strtolower()-funktion dokumentaatio](https://www.php.net/manual/en/function.mb-strtolower.php)