---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "PHP: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad och varför?
Beräknande datum i framtiden eller förflutna innebär att vi manipulerar datum för att skapa nya datum baserat på varierande intervaller. Programmerare gör ofta detta när de skapar schemaläggare, påminnelser eller uppföljningsfunktioner.

## Hur gör man:
PHP har inbyggda funktioner för att utföra sådana datumberäkningar. Låt oss se hur man lägger till och subtraherar dagar från ett datum.

```PHP
<?php
// Lägga till dagar till dagens datum
$futureDate=date('Y-m-d', strtotime('+30 days'));
echo 'Framtida datum: '.$futureDate;

// Dra av dagar från dagens datum
$pastDate=date('Y-m-d', strtotime('-20 days'));
echo 'Informat datum: '.$pastDate;
?>
```
Beroende på det datum du kör denna kod på kommer outputs vara olika. Men den förväntade formatet skulle vara i form av året, månaden och dagen.

## Djupdykning
Historiskt sett tillhandahåller PHP många sätt att hantera datum, inklusive `strtotime()`, `DateTime()`, och `date_add()` / `date_sub()` metoder. De flesta utvecklare föredrar `strtotime()` eller `DateTime` för enkelhet och läsbarhet.

Alternativt kan du använda `DateTime::add()` och `DateTime::sub()` för att lägga till och subtrahera dagar från ett datum. Se hur det fungerar nedan:

```PHP
<?php
$date = new DateTime();
$date->add(new DateInterval('P10D'));
echo 'Dagen efter tio dagar: '.$date->format('Y-m-d');

$date->sub(new DateInterval('P20D'));
echo 'Dagen för tjugo dagar sedan: '.$date->format('Y-m-d');
?>
```

Observera att `P10D` betyder "en period av tio dagar", och du kan ändra dagvärdet efter behov.

## Se även
-Den officiella PHP-dokumentationen: [PHP: Datum / Tid - Manual](https://www.php.net/manual/en/book.datetime.php)
-Kom ihåg att också ta en titt på [W3Schools PHP-datum- och tidsövningar](https://www.w3schools.com/php/php_date.asp) för mer praktisk inlärning.