---
title:                "PHP: Att få den aktuella datumen."
simple_title:         "Att få den aktuella datumen."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna få ut det aktuella datumet är en viktig färdighet för många PHP-utvecklare. Det är användbart för att skapa dynamiskt innehåll på webbplatser, för att spåra användarens aktivitet och för många andra program som kräver att man vet vilket datum det är.

## Hur man gör
För att hämta det aktuella datumet i PHP kan du använda inbyggda funktionen `date()` med ett format som argument. Nedan visas en enkel kod som returnerar dagens datum som en sträng:

```PHP
echo date('Y-m-d'); // Exempel output: 2021-08-23
```

Du kan också ange ett annat format enligt dina behov. Här är ett annat exempel som returnerar datum, dag i veckan och månad:

```PHP
echo date('l, jS F Y'); // Exempel output: Monday, 23rd August 2021
```

Det finns många olika formatalternativ att välja mellan, som visas i PHP:s dokumentation för `date()`-funktionen. Prova dig fram för att hitta det ultimata formatet för dina behov.

## Djupgående
Om du vill ha mer kontroll över det datum som returneras kan du använda andra PHP-funktioner tillsammans med `date()`. Till exempel kan du använda funktionen `strtotime()` för att konvertera en sträng till ett datum.

Här är ett exempel där vi konverterar en sträng med datumet "17th August 2021" till ett datum i UNIX-tidsstämpel-formatet (antal sekunder sedan 1 januari 1970):

```PHP
echo strtotime('17th August 2021'); // Exempel output: 1629148800
```

Sedan kan du använda detta UNIX-tidsstämpel för att ange ett specifikt datum och tid med hjälp av `date()`-funktionen. Detta är användbart om du till exempel vill visa datumet för en specifik händelse oavsett vilket datum det är idag.

## Se även
- [PHP:s dokumentation för date()](https://www.php.net/manual/en/function.date.php)
- [PHP:s dokumentation för strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [Date and Time Functions in PHP](https://www.w3schools.com/php/php_date.asp) (W3Schools)