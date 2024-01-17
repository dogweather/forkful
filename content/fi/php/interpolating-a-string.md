---
title:                "Merkkijonon interpolointi"
html_title:           "PHP: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonon interpolointi on tekniikka, jossa muuttujat tai lausekkeet sisällytetään merkkijonoon. Tätä tehdään helpottaakseen ohjelman kirjoittamista ja luettavuutta sen jälkeen. 

## Kuinka:
```PHP
$name = "Jesse";
$age = 25;

// String interpolointi kahteen eri tapaan
echo "Hei, olen {$name} ja olen {$age} vuotta vanha!";
echo "Hei, olen ".$name." ja olen ".$age." vuotta vanha!";
```

Tulostus:
Hei, olen Jesse ja olen 25 vuotta vanha!

## Syvä sukellus:
Merkkijonon interpolointi oli ensin käytössä Perlissä ja se otettiin käyttöön myös PHP:ssä versiosta 4 alkaen. Tämä tekniikka on kehitetty tekemään merkkijonojen käsittelyä helpommaksi ja tehokkaammaksi. Vaihtoehtona on käyttää merkkijonon yhdistämistä (concatenation), mutta se voi olla hankalampi ja hidastaa suorituskykyä.

## Katso myös:
- [PHP string interpolointi-opas](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.double)
- [Miksi PHP:n merkkijonon interpolointi on parempi vaihtoehto kuin yhdistäminen?](https://stackoverflow.com/questions/28268468/why-is-string-interpolation-in-php-superior-to-string-concatenation)
- [Perl:stä PHP:ksi, s. 794](http://ulf-wendel.de/book/book.pdf)