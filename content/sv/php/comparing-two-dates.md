---
title:                "PHP: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum i PHP kan vara användbart i flera olika situationer, till exempel när man vill kontrollera om ett visst datum redan har passerat eller för att sortera datum i rätt ordning. Det kan även vara användbart i utvecklingen av webbapplikationer där man behöver hantera datum och tider på ett korrekt sätt.

## Så här gör man

För att jämföra två datum i PHP behöver man först skapa två variabler som innehåller de datum man vill jämföra. Detta görs med hjälp av funktionen `date_create()` där man anger datumet som parameter. Sedan kan man använda funktionen `date_diff()` för att jämföra de två datumen och få ut skillnaden mellan dem. Nedan följer ett exempel på kod:

```PHP
$datum1 = date_create("2020-05-15");
$datum2 = date_create("2020-05-20");

$skillnad = date_diff($datum1, $datum2);

echo $skillnad->format("%R%a dagar"); //Skriver ut antalet dagar skillnaden mellan datumen är
```

I detta exempel jämförs datumen 2020-05-15 och 2020-05-20 och resultatet blir +5 dagar eftersom det andra datumet är senare än det första.

## Djupdykning

När man jämför två datum i PHP är det viktigt att förstå hur funktionen `date_diff()` fungerar. Den returnerar en `DateInterval`-objekt som innehåller den faktiska skillnaden mellan datumen. Om man vill ha ett annat format på resultatet kan man använda funktionen `format()` tillsammans med `DateInterval`-objektet för att få ut skillnaden i önskat format. Det finns också flera olika parameteralternativ som man kan använda för att få ut olika delar av skillnaden, till exempel antal år, månader, dagar osv. För att läsa mer om samtliga alternativ och hur man använder `date_diff()` i olika situationer kan man besöka [PHP:s dokumentation](https://www.php.net/manual/en/function.date-diff.php).

## Se även

[Hur man hanterar datum och tider i PHP](https://www.w3schools.com/php/php_date.asp) - Enkel och utförlig guide med kodexempel på hur man kan hantera datum och tider i PHP.

[6 smart tips för hantering av datum och tider i PHP](https://www.cloudways.com/blog/php-date-time/) - En artikel som listar några av de bästa sätten att hantera datum och tider i PHP och ger tips på hur man kan undvika vanliga problem.