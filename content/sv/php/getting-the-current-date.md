---
title:    "PHP: Hämta aktuellt datum"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och visa dagens datum kan vara en användbar funktion för många olika typer av webbplatser och applikationer. Genom att kunna automatiskt hämta det aktuella datumet kan du undvika manuellt arbete och se till att dina användare alltid ser korrekt information.

## Hur man gör

För att hämta det aktuella datumet i PHP använder du funktionen `date()`. Den tar en parameter som består av ett datumformat, som sedan returnerar aktuellt datum i det formatet.

```PHP
echo date("d/m/Y");
```

Detta kommer att ge utdatot "29/06/2021". Du kan även ange andra format såsom "m/d/Y" eller "l, d F Y" för att få det på ett annat sätt.

## Djupdykning

`date()`-funktionen i PHP är väldigt mångsidig och kan användas för att hämta inte bara det aktuella datumet, utan även tider, månader och år. Du kan också använda olika parametrar för att visa datum från en viss tidszon eller på ett annat språk.

Här är några exempel på olika parametrar och format du kan använda med `date()`-funktionen:

- H för 24-timmarsformatet av en timme med nollor framtill (00 - 23)
- i för minuter med nollor framtill (00 - 59)
- l för veckodagen som en fullständig text (exempelvis "tisdag")
- D för veckodagsförkortning (exempelvis "tis")
- F för månadens fullständiga namn (exempelvis "februari")
- n för månadens nummer utan nollor (1 - 12)
- Y för fyrsiffrigt år (exempelvis "2021")
- e för tidszonsidentifierare (exempelvis "Europe/Stockholm")

För en komplett lista över parametrar och format kan du besöka PHP:s dokumentation för `date()`-funktionen.

## Se även

- [PHP:s date()-funktion](https://www.php.net/manual/en/function.date.php)
- [PHP Datum- och tidsfunktioner](https://www.php.net/manual/en/ref.datetime.php)