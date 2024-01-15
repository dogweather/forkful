---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "PHP: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita? 

Säännölliset lausekkeet ovat tehokas työkalu, jota PHP-kehittäjät voivat käyttää tekstien hallintaan ja hakemiseen. Ne mahdollistavat erilaisten merkkijonojen, kuten sähköpostiosoitteiden ja puhelinnumeroiden tarkastamisen ja validoinnin. Ne myös auttavat tekstin käsittelyssä ja muokkaamisessa, mikä säästää aikaa ja vaivaa.

## Näin käytät säännöllisiä lausekkeita PHP:ssä

Säännölliset lausekkeet aloitetaan yleensä merkkijonon alusta suorakaiteenmuotoisilla sulkumerkeillä ```/``` ja loppuu myös sulkumerkkiin. Näiden väliin kirjoitetaan itse säännöllinen lauseke. Esimerkiksi, ```/[0-9]+/```, tämä säännöllinen lauseke etsii kaikki numerot merkkijonosta. 

Säännöllisiä lausekkeita käytettäessä on tärkeä huomioida, että ne ovat case-sensitive eli ne tunnistavat eri kirjainten koon. Esimerkiksi, ```/hello/``` vastaa vain merkkijonoa "hello" eikä "Hello" tai "HELLO". Tämän voi kiertää käyttämällä säännöllisiä lausekkeita, joilla ei ole case-sensitive ominaisuutta, kuten ```/hello/i```. Tämä lauseke vastaa kaikkia vaihtoehtoja: "hello", "Hello" ja "HELLO".

Säännöllisillä lausekkeilla on myös erilaisia metakaraktereita, joilla voidaan haun tarkennusta. Esimerkiksi metakarakteri ```+``` vastaa yhdestä tai useammasta esiintymästä. Joten ```/[0-9]+/``` etsii kaikki numerot merkkijonosta, mutta ```/[0-9]/``` etsii vain yhden numeron.

## Syväsukellus säännöllisiin lausekkeisiin

PHP tarjoaa paljon erilaisia toimintoja säännöllisten lausekkeiden käyttämiseen. Yksi näistä on ```preg_match()``` -funktio, joka tarkistaa vastaako säännöllinen lauseke annettuun merkkijonoon. Tämä funktio palauttaa joko ```true``` tai ```false``` -arvon. 

Toinen hyödyllinen funktio on ```preg_replace()```, joka etsii säännöllisen lausekkeen mukaiset osat ja korvaa ne halutulla tekstillä. Esimerkiksi, ```preg_replace("/hello/", "hei", "hello world")``` palauttaa merkkijonon "hei world".

Myös säännölliset lausekkeet itsessään voivat sisältää ryhmiä, joiden avulla voidaan muodostaa lisätarkennuksia hakuun. Ryhmät toimivat sulkeiden sisällä ja niitä voi käyttää muun muassa haun tulosten tallentamiseen tai uudelleenkäyttöön.

## Katso myös

- [PHP:n virallinen dokumentaatio säännöllisille lausekkeille](https://www.php.net/manual/en/ref.pcre.php)
- [Online työkalu säännöllisten lausekkeiden testaamiseen](https://regex101.com/)