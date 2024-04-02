---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:49.591718-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) PHP:ss\xE4 ovat malleja, joita\
  \ k\xE4ytet\xE4\xE4n merkkijonossa hahmojen yhdistelmien etsimiseen, mahdollistaen\
  \ kehittyneet haku- ja\u2026"
lastmod: '2024-03-13T22:44:56.645903-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) PHP:ss\xE4 ovat malleja, joita\
  \ k\xE4ytet\xE4\xE4n merkkijonossa hahmojen yhdistelmien etsimiseen, mahdollistaen\
  \ kehittyneet haku- ja\u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Mikä & Miksi?

Säännölliset lausekkeet (regex) PHP:ssä ovat malleja, joita käytetään merkkijonossa hahmojen yhdistelmien etsimiseen, mahdollistaen kehittyneet haku- ja korvaustoiminnot sekä datan validoinnin. Ohjelmoijat hyödyntävät regexiä sen tehon ja joustavuuden vuoksi tekstin jäsentämisessä, lomakkeiden validoinnissa tai verkkodatan kaapimisessa, tehden siitä korvaamattoman työkalun kehittäjän työkalupakissa.

## Miten:

PHP tukee säännöllisiä lausekkeita PCRE:n (Perl Compatible Regular Expressions) kirjaston kautta, tarjoten rikkaan joukon funktioita. Näin niitä käytetään:

### Mallin vastaavuuden tarkistaminen:

Jos haluat tarkistaa, esiintyykö malli merkkijonossa, käytä `preg_match()`-funktiota. Tämä funktio palauttaa 1, jos kuvio löytyi merkkijonosta, ja 0, jos ei.

```php
if (preg_match("/\bweb\b/i", "PHP on web-skriptauskieli")) {
    echo "Vastaavuus löytyi.";
} else {
    echo "Vastaavuutta ei löytynyt.";
}
// Tuloste: Vastaavuus löytyi.
```

### Kaikkien vastaavuuksien löytäminen:

`preg_match_all()`-funktiota käytetään, kun tarvitset löytää kaikki kuvion esiintymät merkkijonossa.

```php
$text = "kissat ja koirat";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Tuloste: Array ( [0] => kissat [1] => ja [2] => koirat )
```

### Tekstin korvaaminen:

Tekstin korvaamiseen, joka vastaa säännöllistä lauseketta, käytetään `preg_replace()`-funktiota. Se on uskomattoman tehokas datan muotoilussa ja siivoamisessa.

```php
$originalText = "Huhtikuu 15, 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// Tuloste: Huhtikuu1,2003
```

### Merkkijonojen jakaminen:

Voit jakaa merkkijonon taulukoksi käyttäen `preg_split()`-funktiota, määrittelemällä mallin erottimeksi.

```php
$text = "PHP on, erittäin suosittu, skriptauskieli";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// Tuloste: Array ( [0] => PHP on [1] => erittäin suosittu [2] => skriptauskieli )
```

Lisäksi, monimutkaisia regex-malleja ja tehtäviä varten, kehykset ja kirjastot, kuten Symfonyn `Finder`-komponentti tai Laravelin apufunktioiden kokoelma, saattavat tarjota kätevämmän abstraktiotason. Kuitenkin PHP:n sisäänrakennettujen PCRE-funktioiden ymmärtäminen ja käyttö on elintärkeää tehokkaaseen tekstinkäsittelyyn ja validointiin suoraan PHP-skripteissä.
