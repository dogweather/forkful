---
date: 2024-01-20 17:39:01.037425-07:00
description: "How to: PHP:ss\xE4 merkkijonon voi muuttaa pieniksi kirjaimiksi `strtolower()`-funktiolla.\
  \ T\xE4ss\xE4 helppo esimerkki."
lastmod: '2024-03-13T22:44:56.643191-06:00'
model: gpt-4-1106-preview
summary: "PHP:ss\xE4 merkkijonon voi muuttaa pieniksi kirjaimiksi `strtolower()`-funktiolla."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

## How to:
PHP:ssä merkkijonon voi muuttaa pieniksi kirjaimiksi `strtolower()`-funktiolla. Tässä helppo esimerkki:

```php
<?php
$esimerkkiTeksti = "HEI MAAILMA!";
$pienetKirjaimet = strtolower($esimerkkiTeksti);
echo $pienetKirjaimet; // tulostuu "hei maailma!"
?>
```
Ja toinen esimerkki monikielisillä merkeillä:

```php
<?php
$tervehdys = "HYVÄÄ PÄIVÄÄ!";
$pienennetty = mb_strtolower($tervehdys, 'UTF-8');
echo $pienennetty; // tulostuu "hyvää päivää!"
?>
```

## Deep Dive
Alkuaikoina PHP:ssä oli pelkästään `strtolower()` käytettävissä, mutta se ei tue monikielisiä merkistöjä. Tästä syystä `mb_strtolower()` luotiin, joka toimii useilla kielillä, mukaan lukien esimerkiksi suomi ja muut UTF-8 -enkoodatut merkistöt.

Vaihtoehtoisia tapoja merkkijonon pienentämiseen ovat esimerkiksi `strtoupper()` kääntäen tai `ucwords()` joka muuttaa jokaisen sanan ensimmäisen kirjaimen isoksi. Yksittäisen merkin muuttamiseen voi käyttää `strtolower()` tai `mb_strtolower()` yhdessä `substr()`-funktion kanssa.

Algoritminen toteutus yksinkertaistuu, koska jokaiselle kirjainten välinen koodiero on vakio (esimerkiksi ASCII-koodistossa isojen ja pienten kirjainten välillä on 32 yksikön ero). Kuitenkin, monikielisen tuen ja erikoismerkkien myötä algoritmi voi olla monimutkaisempi.

## See Also
- PHP Manual `strtolower()`: https://www.php.net/manual/en/function.strtolower.php
- PHP Manual `mb_strtolower()`: https://www.php.net/manual/en/function.mb-strtolower.php
- Unicode standard: https://home.unicode.org
- PHP Multibyte String Extension: https://www.php.net/manual/en/book.mbstring.php
