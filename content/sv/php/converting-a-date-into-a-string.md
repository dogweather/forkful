---
title:                "Omvandla ett datum till en sträng"
html_title:           "PHP: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en vanlig uppgift inom webbutveckling. Det är viktigt att kunna göra det korrekt för att kunna visa datum i olika format på en webbsida eller i en applikation. 

## Hur man gör

Det finns flera sätt att konvertera ett datum till en sträng i PHP. Ett enkelt sätt är att använda inbyggda funktionen `date()`. Detta är ett exempel på hur du kan använda den för att få dagens datum i formatet "YYYY-MM-DD":

```PHP
$date = date("Y-m-d");
echo $date; // Outputs: 2021-09-27
```

Du kan också ange ett specifikt datum som ett argument till `date()` istället för att få dagens datum. Till exempel om du vill få datumet för din födelsedag i formatet "DD/MM/YYYY":

```PHP
$birthday = date("d/m/Y", strtotime("27 September 1995"));
echo $birthday; // Outputs: 27/09/1995
```

Det finns också andra användbara formatflaggor som du kan använda med `date()`. Till exempel kan du få hela namnet på månaden istället för förkortningen genom att använda "F":

```PHP
$date = date("F d, Y");
echo $date; // Outputs: September 27, 2021
```

## Djupdykning

När du använder `date()` måste du vara medveten om att den returnerar datum och tid baserat på din servers tidszoninställning. Om du vill använda en annan tidszon kan du använda `date_default_timezone_set()` för att ställa in en annan tidszon innan du använder `date()`.

En annan viktig aspekt är att du måste förstå och följa formatet som anges i dokumentationen för `date()`. Till exempel är "m" för minuter och "M" för månader två helt olika formatflaggor.

## Se även

Här är några länkar till vidare läsning om hur man konverterar datum till strängar i PHP:

- [Date and Time Functions - PHP Manual](https://www.php.net/manual/en/ref.datetime.php)
- [How to Format a Date in PHP - W3Schools](https://www.w3schools.com/php/php_date.asp)
- [Date Formats in PHP - GeeksforGeeks](https://www.geeksforgeeks.org/date-formats-in-php/)