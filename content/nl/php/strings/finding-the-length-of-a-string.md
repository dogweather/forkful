---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:02.950357-07:00
description: 'Hoe: Gebruik de `strlen()` functie zoals dit.'
lastmod: '2024-03-13T22:44:50.885357-06:00'
model: gpt-4-0125-preview
summary: Gebruik de `strlen()` functie zoals dit.
title: De lengte van een string vinden
weight: 7
---

## Hoe:
Gebruik de `strlen()` functie zoals dit:

```php
<?php
$text = "Hello, world!";
$length = strlen($text);
echo $length; // Uitvoer: 13
?>
```

Als je dit uitvoert, zie je `13` op je scherm omdat "Hello, world!" 13 karakters lang is, inclusief de spatie en uitroepteken.

## Diepgaand
De `strlen()` functie is onderdeel van PHP sinds de vroege versies. Het is eenvoudig en werkt op basis van het aantal bytes, wat meestal overeenkomt met het aantal karakters in strings zonder speciale coderingsoverwegingen.

Echter, met de internationalisering van webapplicaties werd het gebruikelijk om met meerdere talen en karaktercoderingen om te gaan. Karakters in UTF-8 kunnen bijvoorbeeld meer dan één byte gebruiken. Dat is waar `mb_strlen()` om de hoek komt kijken:

```php
<?php
// Een string met multibyte karakters
$multibyteText = "こんにちは";
$length = mb_strlen($multibyteText, "UTF-8");
echo $length; // Uitvoer: 5
?>
```

Vijf karakters, maar meer bytes. De `mb_strlen()` functie respecteert karaktercodering en zorgt zo voor nauwkeurige lengtecontroles voor multibyte strings.

`strlen()` is snel en geschikt voor single-byte karaktersets. `mb_strlen()`, hoewel iets langzamer vanwege de noodzaak om met meer complexe codering om te gaan, is noodzakelijk wanneer je werkt met geïnternationaliseerde tekst.

## Zie Ook
- [PHP `strlen()` officiële documentatie](https://www.php.net/manual/en/function.strlen.php)
- [PHP `mb_strlen()` officiële documentatie](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP Multibyte String uitbreiding](https://www.php.net/manual/en/book.mbstring.php)
