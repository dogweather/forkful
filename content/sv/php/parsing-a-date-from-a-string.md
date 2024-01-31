---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:37:48.787185-07:00
simple_title:         "Tolka ett datum från en sträng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att parsra ett datum från en sträng innebär att du översätter text till ett datum-format som PHP kan förstå och arbeta med. Programmerare behöver detta för att hantera datum-indata, som användardata eller tidstämplar från en databas.

## How to:
För att parsra ett datum från en sträng kan du använda `DateTime` klassen eller `strtotime()` funktionen. Här är ett enkelt exempel:

```php
<?php
$datumStrang = "2023-03-15 14:00:00";
$datumObjekt = new DateTime($datumStrang);
echo $datumObjekt->format('Y-m-d H:i:s'); // Utmatning: 2023-03-15 14:00:00

$timestamp = strtotime($datumStrang);
echo date('Y-m-d H:i:s', $timestamp); // Utmatning: 2023-03-15 14:00:00
?>
```

`DateTime` är mer kraftfull och flexibel, men `strtotime()` fungerar bra för enkla situationer och läsbar kod.

## Deep Dive:
Att parsra datum har länge varit en central del av många programmeringsspråk. I PHP har `DateTime` klassen gradvis blivit standard sedan dess introduktion i PHP 5.2.0, och det för att den hanterar olika tidzoner och datumformat väl. Förut använde många den mer begränsade `strtotime()`.

Finns det alternativ? Ja, förutom `DateTime` och `strtotime()`, kan du använda `IntlDateFormatter` för att parsra datum beroende på olika lokaliseringsinställningar. PHP håller koll på sekunderna sedan Unix-epokens start (1 Januari 1970), vilket är viktigt när du behöver jämföra datum eller räkna ut skillnader mellan tider.

## See Also:
- [PHP manual on DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP manual on strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [PHP manual on IntlDateFormatter](https://www.php.net/manual/en/class.intldateformatter.php)
