---
title:                "Kuvion mukaisten merkkien poistaminen"
html_title:           "PHP: Kuvion mukaisten merkkien poistaminen"
simple_title:         "Kuvion mukaisten merkkien poistaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa saattaa olla tarpeen poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi olla hyödyllistä esimerkiksi datan muokkaamisessa tai validoinnissa.

## Kuinka

Seuraavissa esimerkeissä käytetään PHP:n `preg_replace()`-funktiota poistamaan merkkejä, jotka täyttävät halutun kaavan.

````PHP
$lista = array(
    "Omena123",
    "Banaani456",
    "Mansikka789"
);

$kaava = '/[0-9]/'; //poista kaikki numerot

foreach ($lista as $item) {
    $uusi_lista[] = preg_replace($kaava, "", $item);
}

print_r($uusi_lista);

//tulostaa:
//Array (
//    [0] => Omena
//    [1] => Banaani
//    [2] => Mansikka
//)
````

````PHP
$string = "Tämä on teksti, josta haluamme poistaa välilyönnit.";

$kaava = '/\s+/'; //poista välilyönnit

print preg_replace($kaava, "", $string);

//tulostaa:
//Tämäonteksti,jostahaluamme poistaa välilyönnit.
````

## Syvempi sukellus

PHP:n `preg_replace()`-funktio käyttää säännöllisiä lausekkeita (regular expressions) kaavojen määrittämiseen. Säännölliset lausekkeet ovat voimakas työkalu, jolla voidaan tunnistaa ja manipuloida merkkijonoja.

Kaavassa `/[0-9]/` `[0-9]` tarkoittaa "mikä tahansa numero". Vastaavasti `\s+` tarkoittaa "yksi tai useampi välilyönti". Suosittelemme tutustumaan säännöllisiin lausekkeisiin ja niiden erilaisiin käyttötarkoituksiin.

## Katso myös

- [PHP:n virallinen sivusto](https://www.php.net/)
- [Säännöllisten lausekkeiden perusteet](https://www.regular-expressions.info/)
- [PHP:n preg_replace()-funktion dokumentaatio](https://www.php.net/manual/en/function.preg-replace.php)