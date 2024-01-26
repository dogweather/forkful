---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
date:                  2024-01-20T17:31:54.947800-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att beräkna ett datum i framtiden eller förflutet innebär att ta ett befintligt datum och lägga till eller dra ifrån tid. Programmerare gör detta för att hantera bokningar, påminnelser, tidsfrister eller varje gång tid är en viktig faktor i applikationen.

## Hur gör man:
PHP gör datumhantering enkel med inbyggda funktioner. Här är ett snabbt exempel:

```PHP
<?php
$today = new DateTime();
$interval = new DateInterval('P10D'); // 10 dagar

$futureDate = clone $today; // Framtida datum
$futureDate->add($interval);
echo $futureDate->format('Y-m-d') . "\n"; // Visar framtida datum

$pastDate = clone $today; // Förflutet datum
$pastDate->sub($interval);
echo $pastDate->format('Y-m-d') . "\n"; // Visar förflutet datum
?>
```
### Exempelutskrift:
```
2023-04-27  // 10 dagar från idag i framtiden
2023-04-07  // 10 dagar från idag i det förflutna
```

## Djupdykning:
Tid är komplex. PHPs datumhanteringsfunktioner bygger på tidigare C-bibliotek och har utvecklats genom åren för att förenkla komplexiteten i datumberäkningar.

Alternativ för att hantera datum inkluderar `strtotime()`, som är kraftfull för att tolka textbaserade datum:

```PHP
echo date('Y-m-d', strtotime('+10 days')); // Framtida datum
```

Implementeringsdetaljer är viktiga, t.ex. skottår och tidszoner. DateTime-objektet hanterar detta automatiskt. DateTimeImmutable är ett alternativ som förhindrar ändring av det ursprungliga objektet, vilket kan vara praktiskt för att undvika sidoeffekter.

## Se även:
- PHP: Datetime - Manualen (https://www.php.net/manual/en/class.datetime.php)
- PHP: DateInterval - Manualen (https://www.php.net/manual/en/class.dateinterval.php)
- PHP: strtotime - Manualen (https://www.php.net/manual/en/function.strtotime.php)
