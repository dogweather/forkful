---
title:                "Jämföra två datum"
html_title:           "PHP: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra datum är en vanlig uppgift i webbutveckling, särskilt när det gäller att sortera eller filtrera data baserat på datum. Det är också användbart för att beräkna olika tidsintervall eller för att kontrollera om en viss händelse inträffade före eller efter en annan.

## Så här gör du

Att jämföra två datum i PHP är enkelt med hjälp av funktionen `strtotime()` och `date()`. Här är ett exempel där vi jämför två datum och får ut en sträng som visar om det första datumet är före, efter eller samma som det andra datumet.

```PHP
$date1 = strtotime('2020-05-15');
$date2 = strtotime('2020-05-20');

if ($date1 < $date2) {
    echo date('Y-m-d', $date1) . ' är före ' . date('Y-m-d', $date2);
} elseif ($date1 > $date2) {
    echo date('Y-m-d', $date1) . ' är efter ' . date('Y-m-d', $date2);
} else {
    echo date('Y-m-d', $date1) . ' är samma som ' . date('Y-m-d', $date2);
}
```

**Output:**

`2020-05-15 är före 2020-05-20`

Det är viktigt att observera att `strtotime()` konverterar datumet till en tidstämpel, vilket gör det möjligt att utföra jämförelser. Om du vill jämföra två datum som är i olika format, måste du först konvertera dem till tidsstämplar med hjälp av `strtotime()`.

## Djupdykning

När du jämför två datum är det viktigt att tänka på formatering och tidszoner. Om datumet är i ett annat format än vanligtvis används i PHP, eller om du har olika tidszoner i dina två datum, kan det påverka resultatet av din jämförelse.

Några användbara funktioner för att hantera tidszoner i PHP inkluderar `date_default_timezone_set()` som ställer in den aktuella tidszonen och `date_default_timezone_get()` som returnerar den aktuella tidszonen. För att formatera datum på olika sätt kan du använda funktionen `date()` som tar ett formatsträng och en tidsstämpel som argument.

## Se även

* [PHP date()-funktionen](https://www.php.net/manual/en/function.date.php)
* [PHP strtotime()-funktionen](https://www.php.net/manual/en/function.strtotime.php)
* [PHP date_default_timezone_set()-funktionen](https://www.php.net/manual/en/function.date-default-timezone-set.php)