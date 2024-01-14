---
title:                "PHP: Kommenttiriviparametrien lukeminen"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Ennen kuin sukellamme syvemmälle, on hyvä ymmärtää miksi lukisit komentoriviargumentteja PHP-kehittäjänä. Komentoriviargumentit ovat erittäin hyödyllisiä työkaluja, jotka voivat auttaa automatisoimaan prosesseja ja tekemään skriptien suorittamisesta helppoa ja nopeaa.

## Kuinka
PHP:lla on monia tapoja lukea komentoriviargumentteja, mutta yksinkertaisin keino on käyttää `argv`-muuttujaa, joka sisältää taulukon kaikista komentoriviargumenteista. Esimerkiksi:

```PHP
<?php
// Skriptin nimi on ensimmäinen komentoriviargumentti
$script_name = $argv[0];

// Tulostetaan kaikki muut argumentit yksi kerrallaan
for ($i = 1; $i < count($argv); $i++) {
    echo "$argv[$i]\n";
}
```

Jos ajattelit skriptiä `php script.php argument1 argument2`, tulostus olisi seuraava:

```
script.php
argument1
argument2
```

## Syväsukellus
Voit myös käyttää `getopt()`-funktiota lukemaan komentoriviargumentteja nimellä ja arvolla. Tämä on hyödyllistä, kun haluat antaa komentoriville vaihtoehtoja, joilla on määritellyt arvot. Esimerkiksi:

```PHP
<?php
// Asetetaan sallitut vaihtoehdot ja niiden arvot
$shortopts = "f:";
$longopts  = array(
    "file:"
);

// Luetaan argumentit
$options = getopt($shortopts, $longopts);

// Tulostetaan annettu tiedoston nimi
echo "Annitut tiedostonimi: {$options['f']}\n";
echo "Annitut tiedostonimi: {$options['file']}";
```

Jos ajattelit skriptiä `php script.php -f testi.txt --file=testi2.txt`, tulostus olisi seuraava:

```
Annitut tiedostonimi: testi.txt
Annitut tiedostonimi: testi2.txt
```

## Katso myös
- [PHP: `argv` -manuaalisivu](https://www.php.net/manual/en/reserved.variables.argv.php)
- [PHP: `getopt()` -manuaalisivu](https://www.php.net/manual/en/function.getopt.php)
- [Lyhyt ja ytimekäs opas komentoriviargumenttien käyttämiseen PHP-ohjelmoinnissa](https://www.scalingphpbook.com/blog/2012/04/05/using-command-line-arguments-in-php/)