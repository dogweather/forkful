---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi? 

Merkkijonon interpolointi on prosessi, jossa muuttujan arvo sidotaan suoraan merkkijonoon. Tämä tehdään helpottamaan ja nopeuttamaan koodin kirjoitusta ja lukemista.

## Kuinka:

Tarkastele seuraavaa koodinäytettä merkkijonon interpoloinnin käytöstä:

```PHP
<?php
$nimi = "Pekka";
echo "Hei, olen $nimi";
?>
```
Ohjelman tulostus on:

```
Hei, olen Pekka
```

## Syvempi sukellus:

Historiallisesti ottaen, PHP-ohjelmoijat alkoivat käyttää merkkijonon interpolointia, kun huomasivat sen nopeuttavan ohjelmien kirjoittamista ja lisäävän koodin luettavuutta.

Sen sijaan, että käyttäisit pitkää ja sekavaa jonon yhdistämistä, kuten:

```PHP
$nimi = "Pekka";
echo "Hei, olen " . $nimi;
```

Voit käyttää interpolointia parantaaksesi koodin selkeyttä ja tehokkuutta.

Ole kuitenkin tietoinen, että interpolointi voi olla haavoittuvainen SQL-injektiohyökkäyksille, jos et suodata syötteitäsi. 

## Katso myös:

Tässä muutamia linkkejä, jotka voivat auttaa ymmärtämään merkkijonon interpolointia paremmin:

1. [PHP:n viralliset dokumentit](https://www.php.net/manual/en/language.types.string.php)
2. [PHP:n merkkijonon interpolointi Stackoverflow'ssa](https://stackoverflow.com/questions/28267266/php-string-interpolation-vs-concatenation)
3. [Hyviä tapoja estää SQL-injektio PHP:ssa](https://www.w3schools.com/php/php_mysql_prepared_statements.asp)