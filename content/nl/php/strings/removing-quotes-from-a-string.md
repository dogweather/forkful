---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:20.851009-07:00
description: "Het verwijderen van aanhalingstekens uit een string in PHP betekent\
  \ het weghalen van die vervelende dubbele (`\"`) of enkele (`'`) aanhalingstekens\
  \ die je\u2026"
lastmod: '2024-03-13T22:44:50.882491-06:00'
model: gpt-4-0125-preview
summary: "Het verwijderen van aanhalingstekens uit een string in PHP betekent het\
  \ weghalen van die vervelende dubbele (`\"`) of enkele (`'`) aanhalingstekens die\
  \ je\u2026"
title: Quotes verwijderen uit een string
weight: 9
---

## Wat & Waarom?
Het verwijderen van aanhalingstekens uit een string in PHP betekent het weghalen van die vervelende dubbele (`"`) of enkele (`'`) aanhalingstekens die je code logica of database queries kunnen verstoren. Programmeurs doen dit om invoerdata schoon te maken of te desinfecteren, om ervoor te zorgen dat strings veilig gebruikt of opgeslagen kunnen worden.

## Hoe:
Hier is een eenvoudig voorbeeld met behulp van PHP's ingebouwde functies:

```php
$quotedString = "'Hallo,' zei ze, \"Het is een mooie dag!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Geeft uit: Hallo, zei ze, Het is een mooie dag!
```

Eenvoudig, toch? Deze `str_replace()` functie neemt een reeks karakters die uit de string verwijderd moeten worden, inclusief zowel enkele als dubbele aanhalingstekens.

## Diepgaande Duik
Terug in de vroege dagen van PHP, moesten ontwikkelaars extra voorzichtig zijn met aanhalingstekens in strings, vooral bij het invoegen van gegevens in een database. Onjuist afgehandelde aanhalingstekens konden leiden tot SQL injectie-aanvallen. Toen kwamen magische aanhalingstekens, een functie die invoergegevens automatisch 'escaped'. Het werd afgekeurd en uiteindelijk verwijderd omdat het slechte programmeerpraktijken en beveiligingsproblemen aanmoedigde.

Nu gebruiken we functies zoals `str_replace()` of regex met `preg_replace()` voor meer geavanceerde patronen. Hier is een regex voorbeeld:

```php
$quotedString = "'Hallo,' zei ze, \"Het is een mooie dag!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Voor JSON-gegevens, zou je `json_encode()` kunnen gebruiken met opties zoals `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` om extra backslashes in je aanhalingstekens te vermijden.

Bij implementatie, overweeg randgevallen. Wat als je string bepaalde aanhalingstekens moet hebben, zoals dialoog in een verhaal of inches in metingen? Context doet ertoe, dus pas je aanhalingsteken-verwijdering aan op het beoogde gebruik van de gegevens.

## Zie Ook
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL Injectie Preventie](https://owasp.org/www-community/attacks/SQL_Injection)
