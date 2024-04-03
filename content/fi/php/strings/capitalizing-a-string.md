---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:59.961944-07:00
description: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa annetun tekstin ensimm\xE4\
  isen merkin muuttamista suuraakkoseksi, varmistetaan, ett\xE4 lauseet, otsikot tai\u2026"
lastmod: '2024-03-13T22:44:56.639575-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa annetun tekstin ensimm\xE4\
  isen merkin muuttamista suuraakkoseksi, varmistetaan, ett\xE4 lauseet, otsikot tai\
  \ erisnimet alkavat oikein tietoaineistossa."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Kuinka:
PHP tukee natiivisti erilaisia funktioita merkkijonojen alkukirjaimen suurentamiseksi, jokainen palvelee eri tarkoitusta. Tässä on kuinka voit käyttää niitä:

### Merkkijonon ensimmäisen kirjaimen suurentaminen:
```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Tulostaa: Hello, world!
```

### Kunkin sanan ensimmäisen kirjaimen suurentaminen:
```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Tulostaa: Hello, World!
```

### Muuttaa koko merkkijonon suuraakkosiksi:
```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Tulostaa: HELLO, WORLD!
```

Skenaarioille, jotka vaativat enemmän mukauttamista tai kolmannen osapuolen ratkaisuja, voidaan käyttää kirjastoja kuten `mbstring` (monitavuisille merkkijonoille), erityisesti kun käsitellään kansainvälistymistä, jossa merkit voivat ylittää perus ASCII -sarjan.

### Käyttäen mbstring:iä UTF-8 merkkijonojen alkukirjaimen suurentamiseen:
Varmista, että sinulla on `mbstring` -laajennus käytössä PHP-konfiguraatiossasi, sitten:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Tulostaa: Élégant
```

Tämä lähestymistapa auttaa tarkasti suurentamaan merkkijonoja, jotka sisältävät ei-ASCII merkkejä, noudattaen eri kielten hienouksia.
