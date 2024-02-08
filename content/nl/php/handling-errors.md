---
title:                "Fouten afhandelen"
aliases:
- nl/php/handling-errors.md
date:                  2024-01-28T22:02:05.745460-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Foutafhandeling in PHP gaat over het beheren van en reageren op situaties die de normale gang van een programma verstoren, zoals ontbrekende bestanden of slechte data-invoer. Programmeurs verwerken fouten om crashes te voorkomen en gebruikers een soepelere ervaring te bieden.

## Hoe:
In PHP kun je fouten beheren met behulp van `try-catch` blokken, en je kunt het proces aanpassen met aangepaste foutafhandelaars en uitzonderingen.

```php
// Basisvoorbeeld van try-catch
try {
  // Doe iets risicovols
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Verwerk de fout
  echo "Fout: " . $e->getMessage();
}

// Instellen van een aangepaste foutafhandelaar
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Het gebruik van uitzonderingen
class MyException extends Exception {}

try {
  // Doe iets en werp een aangepaste uitzondering
  throw new MyException("Aangepaste foutmelding!");
} catch (MyException $e) {
  // Verwerk de aangepaste uitzondering
  echo $e->getMessage();
}

// Voorbeelduitvoer:
// Fout: fopen(nonexistentfile.txt): mislukt om stream te openen: Bestand of map bestaat niet
// Aangepaste foutmelding!
```

## Diepgaande Duik
Vroeger waren PHP-fouten meer waarschuwingen en mededelingen die de uitvoering van het script niet stopten. Naarmate de taal volwassener werd, nam het robuustere objectgeoriënteerde foutafhandeling aan via de in PHP 5 geïntroduceerde Exception-klasse. Later kwam PHP 7 met foutklassen die eindelijk een onderscheid maakten tussen fouten en uitzonderingen.

Voor `try-catch` blokken, gebruikte PHP `set_error_handler()` om met fouten om te gaan. `try-catch` is schoner, moderner. Maar aangepaste foutafhandelaars hebben nog steeds een plaats, vooral voor legacy code of wanneer je wat normaal gesproken niet-uitzonderlijke fouten moet vangen.

De `Throwable` interface in PHP 7+ betekent dat het nu mogelijk is zowel Errors als Exceptions te vangen. Dit is handig omdat je nu geen kritieke runtime-fouten mist, die voorheen moeilijker te traceren waren.

Alternatieven buiten de ingebouwde mechanismen van PHP omvatten bibliotheken en frameworks die komen met hun eigen foutafhandelingssystemen, en bieden meer functies zoals foutenlogboeken naar bestanden of het weergeven van gebruiksvriendelijke foutpagina's.

## Zie ook
- Officiële PHP-documentatie over uitzonderingen: https://www.php.net/manual/nl/language.exceptions.php
- PHP op de juiste manier over foutenrapportage: https://phptherightway.com/#error_reporting
- PHP Handleiding over foutafhandeling: https://www.php.net/manual/nl/book.errorfunc.php
