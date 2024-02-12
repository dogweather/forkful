---
title:                "Een string interpoleren"
aliases:
- /nl/php/interpolating-a-string.md
date:                  2024-01-28T22:02:26.970658-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Stringinterpolatie stelt u in staat om variabelen direct in een string te injecteren. Programmeurs gebruiken het om variabelen in tekst te verweven, waardoor de code schoner en leesbaarder wordt.

## Hoe:

In PHP kunt u strings interpoleren met behulp van dubbele aanhalingstekens of heredoc-syntax:

```php
$name = "World";
echo "Hallo, $name!"; // Uitvoer: Hallo, World!

// Krullende haakjes gebruiken voor meer complexe variabelen
$object = new stdClass();
$object->greeting = "Hallo";
echo "{$object->greeting}, $name!"; // Uitvoer: Hallo, World!

// Heredoc-syntax voor meerregelige strings
$heredoc = <<<EOT
Dit is een string die $name bevat.
Je kunt hier zoveel schrijven als je wilt.
EOT;
echo $heredoc; // Uitvoer: Dit is een string die World bevat.
```

Opmerking: Enkele aanhalingstekens zullen niet interpoleren:

```php
echo 'Hallo, $name!'; // Uitvoer: Hallo, $name!
```

## Diepgaande Verkenning

Voordat PHP interpolatie introduceerde, was concatenatie met de puntoperator (.) de manier om te gaan. Bijvoorbeeld:

```php
echo 'Hallo, ' . $name . '!';
```

Interpolatie stroomlijnt dit door de variabele direct binnen de string te parsen.

Stringinterpolatie bestaat sinds PHP 4, maar het gebruik van complexe expressies binnen krullende haakjes werd flexibeler met PHP 7. Met deze verbeteringen maakte PHP het gemakkelijker om elke variabele, inclusief objecteigenschappen en array-elementen, binnen een string in te bedden.

Er bestaan alternatieven voor interpolatie, zoals het gebruik van `sprintf()` voor geformatteerde strings of `implode()` voor arrays. Deze kunnen soms meer controle over de stringformat geven, vooral voor lokalisatie en complexe structuren.

Implementatiegewijs zoekt PHP naar variabelen binnen strings wanneer deze tussen dubbele aanhalingstekens of in heredoc-syntax staan en vervangt ze door de waarde van de variabele. De parser negeert het dollarteken ($) in enkelvoudige aanhalingstekens en behandelt het als een gewoon teken.

## Zie Ook

- [PHP: Strings](http://php.net/manual/en/language.types.string.php) - Officiële PHP-documentatie over strings.
- [PHP: Heredoc syntax](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) - Gedetailleerde sectie van de PHP-handleiding over Heredoc.
- [PHP: String Operators](https://www.php.net/manual/en/language.operators.string.php) - Meer over stringconcatenatie en de puntoperator.
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php) - Documentatie van de `sprintf()` functie voor stringformattering.
