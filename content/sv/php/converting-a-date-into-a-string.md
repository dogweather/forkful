---
title:                "PHP: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför konvertera datum till strängar?

Att konvertera ett datum till en sträng är en vanlig uppgift i PHP-programmering. Det kan behövas för att visa när ett visst evenemang eller datum äger rum, eller för att formatera datumet på ett visst sätt för en viss applikation. Att kunna konvertera datumet till en sträng ger också en mer läsbar och användarvänlig presentation av datumen.

## Så här gör du det:

```PHP
$today = date('Y-m-d'); // Skapar en variabel som innehåller dagens datum (i det här fallet i formatet: ÅÅÅÅ-MM-DD)
echo $today; // Ger utmatning: 2021-03-24

$nextWeek = date('l, F jS, Y', strtotime('+1 week')); // Skapar en variabel som innehåller datumet för exakt en vecka framåt
echo $nextWeek; // Ger utmatning: Wednesday, March 31st, 2021
```

I det första exemplet använder vi funktionen `date()` för att skapa en sträng baserad på dagens datum. Vi använder `Y-m-d` för att få utformatet `ÅÅÅÅ-MM-DD`. I det andra exemplet använder vi även funktionen `strtotime()` för att lägga till en vecka på det nuvarande datumet och formaterar sedan strängen med dag, månad och år.

## Utforska mer:

Att konvertera datum till strängar kan vara mer komplicerat än dessa enkla exempel. Det finns flera olika formatvalssträngar som kan användas i funktionen `date()` och det går också att skapa egna format genom att använda delar av datumet och kombinera dem på olika sätt. Det finns också flera andra funktioner som kan användas för att manipulera datum och tider i PHP, som `strtotime()` och `mktime()`. Genom att lära sig mer om dessa funktioner kan du enkelt konvertera datum till önskad form och använda dem i din kod.

## Se även:

- [PHP date() funktionen](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime() funktionen](https://www.php.net/manual/en/function.strtotime.php)
- [PHP mktime() funktionen](https://www.php.net/manual/en/function.mktime.php)