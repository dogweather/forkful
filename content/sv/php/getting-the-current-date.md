---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:15:57.481453-07:00
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Hämta det aktuella datumet i PHP innebär att få information om just nu. Programmerare använder detta för att logga händelser, användardata, eller för att visa tidsspecifik information till användare.

## Hur man gör:
För att få det aktuella datumet och tiden i PHP, använd `date()`-funktionen. Här är ett enkelt exempel:

```PHP
<?php
echo "Idag är det " . date("Y-m-d") . "<br>";
echo "Klockan är " . date("H:i:s");
?>
```

Exempel på utskrift:
```
Idag är det 2023-04-01
Klockan är 12:15:03
```

## Fördjupning:
Funktionen `date()` har funnits sedan PHPs födelse. Från PHP 5.2.0, introducerades klassen `DateTime` som ett mer objektorienterat alternativ. Det finns också `time()` som returnerar en UNIX-tidsstämpel, vilket är antalet sekunder sedan 1970-01-01 00:00:00 UTC.

Implementationsdetaljer kan innehålla arbete med tidszoner, där `date_default_timezone_set()` använder sig av PHPs tidszonsdatabas för att ange relevant tidszon. Alternativa format kan inkludera RFC 2822 eller ISO 8601 datumformat, beroende på behov. 

## Se även:
- PHPs officiella dokumentation om `date()`-funktionen: [php.net/manual/en/function.date.php](https://www.php.net/manual/en/function.date.php)
- PHP `DateTime` klass: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- PHP tidszonsinställningar: [php.net/manual/en/function.date-default-timezone-set.php](https://www.php.net/manual/en/function.date-default-timezone-set.php)
- W3Schools PHP date() funktion: [w3schools.com/php/func_date_date.asp](https://www.w3schools.com/php/func_date_date.asp)
