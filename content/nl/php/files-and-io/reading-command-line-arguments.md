---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:28.327560-07:00
description: 'Hoe: PHP gebruikt een globale array `$argv` om opdrachtregelargumenten
  op te slaan, waarbij `$argv[0]` de scriptnaam is. Zo gebruik je het.'
lastmod: '2024-03-13T22:44:50.911327-06:00'
model: gpt-4-0125-preview
summary: PHP gebruikt een globale array `$argv` om opdrachtregelargumenten op te slaan,
  waarbij `$argv[0]` de scriptnaam is.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe:
PHP gebruikt een globale array `$argv` om opdrachtregelargumenten op te slaan, waarbij `$argv[0]` de scriptnaam is. Zo gebruik je het:

```php
<?php
// controleer of er argumenten zijn doorgegeven
if ($argc > 1) {
    echo "Hallo, " . $argv[1] . "!\n";
} else {
    echo "Hallo, wie je ook bent!\n";
}
?>
```

Als je dit script `sayhello.php` noemt en uitvoert met `php sayhello.php World`, is de uitvoer:

```
Hallo, World!
```

Geen argumenten? Je krijgt:

```
Hallo, wie je ook bent!
```

## Diepgaand
Historisch gezien zijn opdrachtregelscripts de basis van systeemautomatisering geweest, lang voordat GUI's de overhand namen. PHP, hoewel veel gebruikt voor webontwikkeling, biedt ook robuuste CLI-ondersteuning.

Twee hoofdmanieren om argumenten in PHP te lezen zijn `$argv` en de functie `getopt()`. De eerste is een simpele array, terwijl `getopt()` meer complexe functionaliteit biedt, zoals het parsen van opties (met of zonder waarden).

Wat de implementatie betreft, `$argv` en `$argc` (het aantal argumenten) zijn automatisch beschikbaar in CLI-modus - geen extra opzet nodig. Ze zijn niet aanwezig bij het uitvoeren van PHP-webscripts, omdat dat niet hun domein is.

Maar onthoud, als je `argv` en `argc` als globale variabelen registreert via `php.ini` of serverconfiguratie, kunnen ze ook in webscripts worden benaderd. Hoewel, dat is zelden nodig en kan een beveiligingsrisico vormen.

## Zie ook
Voor meer complexe opdrachtregelparsing:
- [PHP.net getopt](https://www.php.net/manual/en/function.getopt.php)

Om dieper in te gaan op PHP's CLI-server:
- [PHP.net Opdrachtregelgebruik](https://www.php.net/manual/en/features.commandline.php)

Ga in gesprek met de PHP-community:
- [PHP CLI-discussies op Stack Overflow](https://stackoverflow.com/questions/tagged/php+cli)
