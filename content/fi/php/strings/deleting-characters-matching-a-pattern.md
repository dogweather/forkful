---
date: 2024-01-20 17:42:48.527320-07:00
description: "How to: - Kuinka: PHP:ss\xE4 `preg_replace`-funktio on yleisin tapa\
  \ poistaa merkkej\xE4, jotka vastaavat tietty\xE4 mallia. Katsotaanpa miten."
lastmod: '2024-03-13T22:44:56.640573-06:00'
model: gpt-4-1106-preview
summary: "PHP:ss\xE4 `preg_replace`-funktio on yleisin tapa poistaa merkkej\xE4, jotka\
  \ vastaavat tietty\xE4 mallia."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: - Kuinka:
PHP:ssä `preg_replace`-funktio on yleisin tapa poistaa merkkejä, jotka vastaavat tiettyä mallia. Katsotaanpa miten:

```PHP
<?php
$originalString = 'Hello, this is a test! 123.';
$pattern = '/[0-9]+/'; // Poistetaan kaikki numerot.

$cleanedString = preg_replace($pattern, '', $originalString);

echo $cleanedString; // Output: Hello, this is a test! .
?>
```

`$pattern` määrittää poistettavat merkit, tässä tapauksessa kaikki numerot. `preg_replace` korvaa ne tyhjällä merkkijonolla.

## Deep Dive - Syväsukellus:
`preg_replace` perustuu Perl-yhteensopiviin säännöllisiin lausekkeisiin (Perl Compatible Regular Expressions, PCRE), joita on käytetty PHP:ssä pitkään. Historiallisesti PHP on tarjonnut muitakin funktioita, kuten `ereg` (nyt vanhentunut), mutta PCRE on nykyään standardi.

Vaihtoehtoisesti voi käyttää `str_replace`, jos tarvitsee vain yksinkertaista merkkijonon korvaamista ilman mallien käyttöä. Suorituskyvyssä `preg_replace` voi olla hitaampi, mutta se on paljon joustavampi mallin mukaisten merkkijonojen käsittelyssä.

## See Also - Katso Myös:
- PHP.net - PCRE dokumentaatio: https://www.php.net/manual/en/book.pcre.php
- PHP.net - `preg_replace` funktio: https://www.php.net/manual/en/function.preg-replace.php
- Säännölliset lausekkeet perusteet: https://www.regular-expressions.info/tutorial.html
