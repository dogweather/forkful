---
title:                "Lausekkeen muuttaminen pieniksi kirjaimiksi"
html_title:           "PHP: Lausekkeen muuttaminen pieniksi kirjaimiksi"
simple_title:         "Lausekkeen muuttaminen pieniksi kirjaimiksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Mikäli haluat muuttaa tekstin merkkejä pieniksi isoista kirjaimista, on PHP:n stringin muuntaminen pieniksi kirjaimiksi erittäin hyödyllinen toiminto. Tämä helpottaa esimerkiksi tiedon vertailua ja lajittelua.

## Kuinka

```PHP
$text = "TÄMÄ TEKSTI ON ISOILLA KIRJAIMILLA!";
echo strtolower($text);
```
Tulostus:
```
tämä teksti on isoilla kirjaimilla!
```
## Deep Dive

PHP:n `strtolower()`-funktio muuttaa annetun stringin kaikki merkit pieniksi kirjaimiksi. Se myös osaa käsitellä Unicode-merkkejä, kuten ääkkösiä ja muita erikoismerkkejä. Tämä tekee siitä erittäin kätevän muunnostyökalun monikielisille sivustoille.

Tärkeä huomioitava asia on, että `strtolower()` käyttää oletuksena ISO-8859-1 merkistökoodausta, joten mikäli käytät esimerkiksi UTF-8 merkistöä, täytyy asettaa oikea merkistökoodaus parametrina `strtolower()`-funktiolle.

## Katso myös

- [PHP:n string-funktiot](https://www.php.net/manual/en/ref.strings.php)
- [`strtolower()`-funktion dokumentaatio](https://www.php.net/manual/en/function.strtolower.php)
- [Merkit ja merkistökoodaukset](https://www.w3.org/International/articles/definitions-characters/)