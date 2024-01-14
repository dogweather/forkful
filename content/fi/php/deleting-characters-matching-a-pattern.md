---
title:    "PHP: Kuviota vastaavien merkkien poistaminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointitehtävissä on tarve poistaa tiettyä kuvioita vastaavia merkkejä tekstistä, esimerkiksi poistaa kaikki välilyönnit tai poistaa merkkejä, jotka eivät ole numeroita. Tässä blogikirjoituksessa opimme kuinka voi poistaa merkkejä jokaisen ohjelmoijan työkalupakista löytyvällä PHP-kielellä.

## Kuinka tehdä

Yksinkertaisin tapa poistaa merkit tietystä kuvioista on käyttää PHP:n sisäänrakennettuja funktioita. Esimerkiksi, jos haluamme poistaa kaikki välilyönnit yhdestä merkkijonosta, voimme käyttää `str_replace()` -funktiota seuraavasti:

```PHP
$teksti = "Tämä on esimerkki tekstillä, jossa on välilyöntejä.";
$puhdas_teksti = str_replace(' ', '', $teksti);

echo $puhdas_teksti; // tulostaa: Tämäonesimerkkitekstillä,jossaonvälilyöntejä.
```

Jos taas haluamme poistaa kaikki merkit, jotka eivät ole numeroita, voimme käyttää `preg_replace()` -funktiota säännöllisten lausekkeiden avulla:

```PHP
$teksti = "12345#6a7b8c9d";
$puhdas_teksti = preg_replace('/[^0-9]/', '', $teksti);

echo $puhdas_teksti; // tulostaa: 123456789
```

Voit myös sisällyttää säännöllisen lausekkeen suoraan käyttäen `preg_match()` -funktiota. Tämä esimerkki näyttää kuinka tulostetaan vain numeromerkit merkkijonosta:

```PHP
$teksti = "12345#6a7b8c9d";

preg_match_all('/[0-9]/', $teksti, $tulokset);

echo implode($tulokset[0]); // tulostaa: 123456789
```

## Syvällinen sukellus

PHP:ssa on muitakin hyödyllisiä funktioita, kuten `trim()` joka poistaa merkit merkkijonon alusta ja lopusta tai `mb_ereg_replace()` joka käsittelee monikielisiä merkkijonoja.

Lisäksi voit käyttää säännöllisiä lausekkeita monipuolisemmin, kuten erilaisia metakaraktereja ja määrittelyjä. Hyödyllisiä säännöllisen lausekkeen esimerkkejä löytyy esimerkiksi [PHP:n viralliselta sivustolta](https://www.php.net/manual/en/regexp.reference.escape.php).

## Katso myös

- [PHP:n string-funktiot](https://www.php.net/manual/en/ref.strings.php)
- [Säännölliset lausekkeet PHP:ssa](https://www.php.net/manual/en/book.pcre.php)