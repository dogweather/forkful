---
title:                "PHP: Få aktuellt datum"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få dagens datum är en väldigt användbar och vanlig funktion inom PHP programmeringsvärlden. Detta är särskilt användbart för webbutvecklare som behöver visa aktuellt datum på sina webbsidor. Det finns många olika sätt att få fram dagens datum i PHP, och det är viktigt att förstå varför det är så viktigt att veta hur man gör det.

## Så här gör du

Ett enkelt sätt att få dagens datum är genom att använda inbyggda PHP-funktionen `date()`. Detta gör det möjligt att specificera ett format för datumet som ska returneras. Till exempel kan du använda följande kod för att få fram dagens datum i det vanliga formatet "dag/månad/år":

```PHP
<?php
    echo "Dagens datum är " . date("d/m/Y") . "<br>";
?>
```

Det finns också möjlighet att använda formatet "år-månad-dag", vilket följer ISO-standard för datumformat. Detta kan användas för att sortera datum i kronologisk ordning inom en databas. Exempel på kod för att få fram dagens datum i detta format är:

```PHP
<?php
    echo "Dagens datum är " . date("Y-m-d") . "<br>";
?>
```

Om du vill inkludera dagens veckodag i datumet så kan du använda formatet "dag/månad/år (veckodag)". Detta kan göras genom att lägga till en extra parameter i `date()` funktionen. Exempel på kod för detta är:

```PHP
<?php
    echo "Dagens datum är " . date("d/m/Y (l)") . "<br>";
?>
```

Det finns också möjlighet att få fram andra tids- och datumrelaterade information såsom aktuellt år, månad, dag, vecka, timme, minut och sekund. Detta kan åstadkommas genom att använda inbyggda PHP-funktioner såsom `date("Y")` för att få fram aktuellt år eller `date("m")` för att få fram aktuell månad.

## Djupdykning

Som nämnts tidigare finns det många olika sätt att få fram dagens datum i PHP. En annan vanlig metod är att använda funktionen `strtotime()` som konverterar en textrepresentation av ett datum till en numerisk representation som sedan kan formateras med `date()` funktionen.

Ett exempel på detta är att få fram datumet för nästa vecka genom att använda följande kod:

```PHP
<?php
    $date = strtotime("+1 week");
    echo "Datumet för nästa vecka är " . date("d-m-Y", $date);
?>
```

Det finns många andra användbara funktioner och alternativ för att få fram dagens datum i PHP och det är viktigt att utforska dem för att hitta den bästa lösningen för din kod.

## Se även

- [PHP officiell dokumentation för date() funktionen](https://www.php.net/manual/en/function.date.php)
- [PHP officiell dokumentation för strtotime() funktionen](https://www.php.net/manual/en/function.strtotime.php)
- [Guide för att använda datum och tid i PHP](https://www.w3schools.com/php/php_date.asp)