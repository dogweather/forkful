---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
aliases:
- fi/php/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:48.527320-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Merkkien poistaminen kuvion mukaan tarkoittaa sellaisten merkkijonojen osien hävittämistä, jotka vastaavat tiettyä kaavaa. Ohjelmoijat tekevät tätä syistä, kuten datan puhdistaminen, tietoturva, tai syötteen käsittelyn yksinkertaistaminen.

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
