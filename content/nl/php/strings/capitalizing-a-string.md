---
title:                "Een string met hoofdletters maken"
aliases:
- /nl/php/capitalizing-a-string/
date:                  2024-01-28T21:55:59.642219-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het kapitaliseren van een string betekent het veranderen van de eerste letter van elk woord naar een hoofdletter. Programmeurs kapitaliseren strings voor visuele consistentie, branding of ontwerp van de gebruikerservaring.

## Hoe te:
In PHP kapitaliseren we strings met de functie `ucwords()` voor volledige titels of `ucfirst()` voor enkele regels of zinnen.

```php
<?php
$lowercase_title = "the quick brown fox jumps over the lazy dog";
$capitalized_title = ucwords($lowercase_title);

echo $capitalized_title; // Resultaat: The Quick Brown Fox Jumps Over The Lazy Dog

$sentence = "an example sentence.";
$capitalized_sentence = ucfirst($sentence);

echo $capitalized_sentence; // Resultaat: An example sentence.
?>
```

## Diepduiken
Het kapitaliseren van strings is geen nieuw concept. In de drukwereld is titelkapitalisatie een standaardconventie. In PHP zijn `ucwords` en `ucfirst` al een tijdje beschikbaar, waardoor dergelijke conventies digitaal mogelijk zijn. PHP's `mb_convert_case` functie maakt complexere manipulaties mogelijk, zoals `MB_CASE_TITLE`, vooral nuttig voor multibyte (niet-ASCII) strings.

Alternatieven voor `ucwords` zijn onder meer `strtoupper`, dat de hele string naar hoofdletters omzet, en `strtolower`, dat de string naar kleine letters omzet. Let op de locatie: bepaalde talen hebben unieke kapitalisatieregels.

Wat betreft de implementatie past `ucwords` hoofdletters toe op het eerste teken na een whitespace, niet alleen spaties. Dit betekent dat nieuwe regels, tabbladen, etc., allemaal kapitalisering activeren.

## Zie ook
Voor meer informatie bekijk:

- PHP Handleiding over `ucwords()`: https://www.php.net/manual/en/function.ucwords.php
- PHP Handleiding over `ucfirst()`: https://www.php.net/manual/en/function.ucfirst.php
- PHP Handleiding over `mb_convert_case()`: https://www.php.net/manual/en/function.mb-convert-case.php
- PHP stringfuncties: https://www.php.net/manual/en/ref.strings.php
