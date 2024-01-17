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

## Mitä & Miksi?
Säännöllisten lausekkeiden käyttäminen on tapa tarkistaa ja muokata tekstiä tietyllä kaavalla. Ohjelmoijat tekevät niin helpottamaan tiettyjen kuvioitten ja sääntöjen tunnistamista ja muokkaamista suurista tekstimääristä.

## Miten:
```PHP
$string = "Tämä on esimerkkiteksti!";
$result = preg_match("/[a-zA-Z]+/", $string);
echo $result; // outputs "1"
```

Säännöllisissä lausekkeissa käyttämämme ```preg_match``` -funktio tarkistaa, täsmääkö annettu teksti haluamaamme kaavaan. Tässä esimerkissä on tarkistettu, sisältääkö merkkijono vähintään yhden aakkosen. Olemme käyttäneet ```[a-zA-Z]+``` kaavaa, joka tarkoittaa "yhtä tai useampaa pienaakkosta tai isoaakkosta". Funktio palauttaa arvon "1" jos lauseke löytyy tekstistä ja "0" jos ei.

## Syventävä tarkastelu:
Säännölliset lausekkeet ovat olleet käytössä jo vuosikymmeniä ja niitä käytetään monissa ohjelmointikielissä, myös PHP:ssa. Niitä käytetään esimerkiksi validointiin, tekstien muokkaamiseen ja tietojen hakuun. Toisinaan säännöllisiä lausekkeita voidaan korvata muilla menetelmillä, kuten PHP:n sisäänrakennetulla ```strpos``` -funktiolla, joka etsii merkkijonosta halutun tekstinpätkän. Säännölliset lausekkeet vaativat harjoittelua ja niiden käyttö on yleensä helpompaa, kun ymmärrät niiden toimintaperiaatteen.

## Katso myös:
- [W3Schools: PHP Regular Expressions](https://www.w3schools.com/php/php_regex.asp)
- [PHP.net: Regular Expressions](https://www.php.net/manual/en/book.regex.php)