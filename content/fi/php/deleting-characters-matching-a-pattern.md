---
title:                "PHP: Kuvioon sopivien merkkien poistaminen"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Tämä on hyvä tapa puhdistaa syötteitä tai tekstidataa, joka saattaa sisältää tarpeettomia merkkejä.

## Miten tehdä

```PHP
$text = "Tämä on esimerkki tekstistä! Tässä on ekstra merkki.";
$modified_text = preg_replace("/[!]/", "", $text);
echo $modified_text;
```
Tulostus:

```
Tämä on esimerkki tekstistä Tässä on ekstra merkki.
```

Koodiesimerkissä käytämme `preg_replace()` -funktiota, joka ottaa ensimmäisenä parametrina vastaan kaavan, jonka haluamme poistaa. Tässä tapauksessa se on `/[!]/`, mikä tarkoittaa, että haluamme poistaa kaikki huutomerkki-merkit tekstistä. Toisena parametrina annamme tyhjän merkkijonon, mikä tarkoittaa, että poistetut merkit korvataan tyhjällä.

## Syvempi sukellus

Voit myös käyttää säännöllisiä lausekkeita (regular expressions) kaavassa, jolla poistetaan merkkejä. Tässä esimerkissä haluamme poistaa kaikki numerot tekstistä:

```PHP
$text = "12 varista laulaa puussa.";
$modified_text = preg_replace("/[0-9]/", "", $text);
echo $modified_text;
```
Tulostus:

```
varista laulaa puussa.
```

Säännölliset lausekkeet ovat hyödyllisiä, koska ne antavat enemmän joustavuutta ja tarkkuutta kaavassa. Voit löytää lisätietoa PHP:n säännöllisistä lausekkeista PHP:n virallisesta dokumentaatiosta.

## Katso myös

- PHP:n viralliset dokumentaatiot säännöllisistä lausekkeista: https://www.php.net/manual/en/regexp.reference.php 
- Hyödyllisiä vinkkejä säännöllisten lausekkeiden käyttöön: https://www.regular-expressions.info/
- Leikkaa ja liitä -työkalu säännöllisten lausekkeiden testaamiseen: https://regex101.com/