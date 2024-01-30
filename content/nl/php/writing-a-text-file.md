---
title:                "Een tekstbestand schrijven"
date:                  2024-01-28T22:12:56.370471-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand schrijven"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand schrijven in PHP gaat over het opslaan van gegevens naar een bestand op de server. Programmeurs doen dit vaak voor datalogging, configuratie-instellingen, of het exporteren van gegevens die bedoeld zijn om gelezen te worden door mensen of andere systemen.

## Hoe:

Schrijven naar een bestand in PHP kan zo simpel zijn als het gebruiken van de `file_put_contents()` functie, die een bestandsnaam en een reeks gegevens neemt. Hier is een snel voorbeeld:

```php
<?php
$data = "Hallo, wereld!\n";
file_put_contents("voorbeeld.txt", $data);
?>
```

Dit script uitvoeren creëert "voorbeeld.txt" met de inhoud "Hallo, wereld!".

Voor meer controle kun je een bestand openen, er naar schrijven, en het dan sluiten:

```php
<?php
$file = fopen("voorbeeld.txt", "w") or die("Kan bestand niet openen!");
$txt = "Hallo nog eens, wereld!\n";
fwrite($file, $txt);
fclose($file);
?>
```

Beide scripts resulteren in dezelfde uitvoer in "voorbeeld.txt".

## Diepgaand

Historisch gezien boden PHP's `fopen()`, `fwrite()`, en `fclose()` gedetailleerde controle voor bestandschrijfbewerkingen, zoals toevoegen of vergrendelen. `file_put_contents()` werd geïntroduceerd in PHP 5 voor een vereenvoudigde benadering.

Alternatieven omvatten het gebruik van `fputcsv()` voor het genereren van CSV-bestanden of de `SplFileObject` klasse voor object-georiënteerde bestandsbewerkingen. Implementatiedetails omvatten het afhandelen van bestandsrechten en het zorgen voor uitzonderingsafhandeling of foutcontrole met `or die()` of `try-catch` blokken.

## Zie Ook

- [PHP file_put_contents()](https://www.php.net/manual/nl/function.file-put-contents.php)
- [PHP fopen()](https://www.php.net/manual/nl/function.fopen.php)
- [PHP fwrite()](https://www.php.net/manual/nl/function.fwrite.php)
- [PHP-bestandsafhandeling](https://www.php.net/manual/nl/book.filesystem.php)
- [Bestandsrechten begrijpen](https://www.php.net/manual/nl/function.chmod.php)