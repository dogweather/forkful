---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:56:27.568862-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
`## Mitä & Miksi?`

Komennonriviparametrien lukeminen tarkoittaa käyttäjän antamien argumenttien vastaanottamista suoraan ohjelmaan. Tätä tehdään, koska se on nopea tapa personalisoida ohjelman toimintaa ilman GUI:n käyttöä.

## How to:
`## Kuinka:`

PHP:n komentoriviskripteissä argumentteja luetaan `$argv` muuttujasta, joka on osa jokaista skriptiä. Ole kliffa ja tsekkaa nämä esimerkit:

```PHP
<?php
// Tarkistetaan onko argumentteja annettu
if ($argc > 1) {
    echo "Hei! Ensimmäinen argumentti oli: " . $argv[1] . "\n";
} else {
    echo "Anna jokin argumentti komennolla.\n";
}
?>
```
Jos ajetaan skripti komennolla `php script.php Terve`, tulostuu:

```
Hei! Ensimmäinen argumentti oli: Terve
```

## Deep Dive
`## Syväluotaus:`

PHP:ssä `$argv` ja `$argc` tuli käyttöön jo aikaisissa versioissa, mahdollistaen komentoriviparametrien käsittelyn. `$argv` on taulukko, joka sisältää kaikki komentoriviltä annetut argumentit, ja `$argc` on numero, joka kertoo argumenttien lukumäärän.

Vaihtoehtoja on. PHP:n `getopt()` funktio on hienostuneempi tapa käsitellä argumentteja, ja se tukee optioita kuten liput ja nimetyt arvot.

Tärkeää on ymmärtää, että `$argv[0]` on aina skriptin nimi, joten oikeat argumentit alkavat indeksistä 1.

## See Also
`## Katso Myös:`

Lisätietoja ja syvällisempää perehtymistä varten:

- PHP:n viralliset dokumentaatiot `$argv` ja `$argc` käytöstä: [php.net/manual/en/reserved.variables.argv.php](https://www.php.net/manual/en/reserved.variables.argv.php)
- `getopt()` PHP-funktio: [php.net/manual/en/function.getopt.php](https://www.php.net/manual/en/function.getopt.php)
- Komennonrivityökalujen kehittäminen PHP:llä: [symfony.com/doc/current/components/console.html](https://symfony.com/doc/current/components/console.html) (Jos haluat mennä todella professional-tasolle.)
