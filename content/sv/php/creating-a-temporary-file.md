---
title:    "PHP: Skapa en temporär fil"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Varför

Att skapa en tillfällig fil kan vara en viktig del av en programmerares arbetsflöde. Det kan hjälpa till att hantera temporära data och säkerställa att allt fungerar smidigt inom din applikation.

## Så här

Skapa en tillfällig fil är relativt enkelt i PHP. Du kan använda funktionen `tmpfile()` för att skapa en fil som automatiskt hanteras av operativsystemet och raderas när den inte längre behövs. Här är ett exempel:

```PHP
<?php
$file = tmpfile();
fwrite($file, "Detta är en temporär fil.");
echo ftell($file); // Output: 24 (antalet tecken som skrivits till filen)
fclose($file); // Filen kommer automatiskt att raderas här
?>
```
Om du vill ändra namnet på den temporära filen kan du använda funktionen `tempnam()`. Det här kan vara användbart om du behöver hänvisa till filen senare i din kod. Här är ett exempel:

```PHP
<?php
$tempfile = tempnam("/tmp", "TEMP_"); // Skapar en fil med namnet "TEMP_12345" i mappen "/tmp"
$file = fopen($tempfile, "w"); // Öppnar filen i skrivläge
fwrite($file, "Detta är en annan temporär fil.");
fclose($file);
echo file_get_contents($tempfile); // Output: "Detta är en annan temporär fil."
unlink($tempfile); // Filen raderas manuellt här
?>
```

## Djupdykning

Det finns flera anledningar till att man vill skapa en tillfällig fil. Att man behöver hantera temporär data är en vanlig anledning, till exempel när man arbetar med filer som är för stora för att lagras i minnet. Att skapa en tillfällig fil kan också användas för att testa viss funktionalitet utan att riskera att påverka befintliga filer.

När du skapar en tillfällig fil i PHP är den automatiskt skyddad mot att återgå till användaren. Detta innebär att andra användare på servern inte kommer att kunna komma åt din fil. Det är också viktigt att notera att det inte finns något garantier för hur länge en tillfällig fil kommer att finnas kvar. Det kan variera beroende på operativsystem och serverinställningar.

## Se också

- [PHP-funktioner för tillfälliga filer](https://www.php.net/manual/en/ref.filesystem.php)
- [Hur man hanterar filer i PHP](https://www.php.net/manual/en/function.file.php)
- [Tips för effektiv PHP-programmering](https://www.php-fig.org/psr/psr-2/)

Det är viktigt att förstå hur man effektivt kan hantera tillfälliga filer i PHP för att kunna skriva robust och smidig kod. Genom att använda de funktioner som finns tillgängliga kan du enkelt hantera dina temporära data och undvika onödig komplexitet.