---
title:                "Hämta nuvarande datum"
html_title:           "PHP: Hämta nuvarande datum"
simple_title:         "Hämta nuvarande datum"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den aktuella datumen är ett vanligt uppgift för många programmerare. Genom att hämta det aktuella datumet från systemet, kan man skapa dynamiska applikationer som visar exakta datum och tidsdata baserat på användarens lokala inställningar. Detta är särskilt användbart för skapande av online kalendrar, bokningssystem och andra webbapplikationer som är beroende av korrekt tidsdata.

## Hur man gör:
För att hämta den nuvarande datumen i PHP, används den inbyggda "date" funktionen. Detta är ett exempel på hur man använder funktionen och vad som skulle visas som utgång:

```PHP
$today = date("d/m/Y"); 
echo "Idag är det: " . $today . "<br>"; 
```
Output: Idag är det 30/11/2020

Du kan även ändra formatet på datumet baserat på dina behov genom att ändra parametrarna i "date" funktionen. Här är ett annat exempel där vi visar en tidsstämpel tillsammans med datumet:

```PHP
$timestamp = time(); 
$today = date("d/m/Y H:i:s", $timestamp); 
echo "Datum och tid är: " . $today . "<br>"; 
```
Output: Datum och tid är: 30/11/2020 16:32:14

## Djupdyka:
För att ytterligare anpassa tidsdata från systemet, kan du använda funktionen "strtotime" för att konvertera datumet till en Unix-tidsstämpel. Detta gör det möjligt att utföra matematiska operationer med datum, som att lägga till eller ta bort dagar. Det finns också alternativa sätt att hämta den nuvarande datumen i PHP, som att använda funktionen "gmdate" för att hämta tid i GMT-format eller "strftime" för att anpassa utdataformatet.

## Se även:
- [PHP.net Date function](https://www.php.net/manual/en/function.date.php)
- [PHP.net Strtotime function](https://www.php.net/manual/en/function.strtotime.php)
- [W3Schools Date function tutorial](https://www.w3schools.com/php/php_date.asp)