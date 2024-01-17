---
title:                "Kommentoriviparametrien lukeminen"
html_title:           "PHP: Kommentoriviparametrien lukeminen"
simple_title:         "Kommentoriviparametrien lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & miksi?

Lue komentoriviparametrit on prosessi, jossa ohjelmoija lukee käyttäjän antamia komentoja koodin suorittamiseksi. Tämä on tärkeä osa ohjelmointia, koska se mahdollistaa käyttäjälle tietynlaisen interaktion ohjelman kanssa ja antaa enemmän joustavuutta ohjelman suorittamiseen.

## Miten:

```PHP
// Otetaan talteen käyttäjän antamat komentoriviparametrit
$parametri = $argv[1];

echo "Hei, " . $parametri;
```
**Output**: *Hei, [käyttäjän antama nimi]*

Käyttäjä voi antaa komentorivillä haluamansa parametrin, joka tallentuu muuttujaan ja näytetään lopullisessa tulosteessa. Tämä mahdollistaa esimerkiksi henkilökohtaisemman tervehdyksen käyttäjälle.

## Syvemmältä:

Komentoriviparametrien lukeminen on ollut osa ohjelmointia jo kauan. Ennen PHP:n kehitystä tätä tehtiin esimerkiksi C-kielellä. Näin ollen, jos osaat lukea komentoriviparametrejä PHP:ssä, voit hyödyntää tätä taitoa myös muilla kielillä.

Vaihtoehtoisesti, käyttäjä voi myös antaa syötteen esimerkiksi PHP-skriptin luomalla lomakkeella. Tämä on yleisempää web-sovelluksissa, joissa käyttäjä voi antaa syötteen esimerkiksi painamalla lomakkeen submit-nappia.

Komentoriviparametrien lukeminen PHP:ssä tapahtuu käyttämällä `$argv`-muuttujaa, joka on PHP:n sisäänrakennettu muuttuja. Tämä sisältää kaikki käyttäjän antamat parametrit, joita voidaan sitten käsitellä halutulla tavalla.

## Katso myös:

- [PHP: Command Line Usage](https://www.php.net/manual/en/features.commandline.usage.php)
- [PHP: POST Request](https://www.php.net/manual/en/reserved.variables.post.php)
- [C - Command Line Arguments](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)