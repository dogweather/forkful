---
title:    "PHP: Att få dagens datum"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få dagens datum är en viktig del av många webbutvecklarars dagliga rutin. Det kan vara användbart för att visa aktuell information på en webbplats eller för att spåra aktiviteter inom ett projekt.

## Hur du gör det

Enklaste sättet att få aktuellt datum i PHP är genom att använda funktionen `date()`. Detta är ett exempel på hur du kan använda den:

```PHP
$current_date = date('Y-m-d');
echo $current_date;
```

Outputten för detta kodblock kommer att vara dagens datum i formatet "åååå-mm-dd".

Det finns många olika format som du kan använda för att få datumen på olika sätt. Till exempel kan du lägga till tiden i formatet genom att inkludera ett "H:i:s" efter datumet som i följande exempel:

```PHP
$current_date_and_time = date('Y-m-d H:i:s');
echo $current_date_and_time;
```

Outputten kommer nu att innehålla både datum och tid i formatet "åååå-mm-dd hh:mm:ss" (där hh är timmar, mm är minuter och ss är sekunder).

Det finns även möjlighet att använda andra funktioner för att få dagens datum, såsom `getDate()` som returnerar en array med många olika delar av datumet som år, månad och dag. Du kan läsa mer om dessa funktioner i PHP:s officiella dokumentation.

## Deep Dive

Att få dagens datum kan även vara användbart för att gå djupare in i tidsuppgifter. Du kan använda PHP-funktionen `strtotime()` för att konvertera ett datum till en Unix-timestamp (antalet sekunder som har gått sedan den 1 januari 1970). Sedan kan du använda andra tidsfunktioner för att utföra beräkningar eller manipulationer av datumen.

En annan intressant funktion är `strftime()` som låter dig formatera en tidsstämpel enligt specifika regionella inställningar. Du kan exempelvis få dagens datum skrivet på svenska med hjälp av denna funktion.

## Se även

- PHP:s officiella dokumentation om datum och tid: https://www.php.net/manual/en/book.datetime.php
- Konvertera datum till Unix-timestamp: https://www.w3schools.com/php/php_ref_date.asp
- Mer detaljerad information om tidsuppgifter i PHP: https://www.geeksforgeeks.org/php-timestamping/