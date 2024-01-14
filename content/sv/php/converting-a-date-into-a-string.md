---
title:                "PHP: Omvandla ett datum till en sträng"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en viktig del av programmering eftersom det gör det möjligt för användaren att läsa och förstå datumen i ett format som är lättare att läsa och använda.

## Hur

```PHP
$date = date_create('2021-09-01');
echo date_format($date, 'd M Y');
```
Output: 01 Sep 2021

Det första steget är att skapa ett datumobjekt med hjälp av funktionen `date_create()`. Sedan använder vi `date_format()` funktionen för att konvertera datumet till önskat format, i detta fall "d M Y" som står för dag, månad och år. 

```PHP
$date = date_create('2021-09-01');
echo date_format($date, 'l, jS F Y');
```
Output: Wednesday, 1st September 2021

Förutom att konvertera datum till olika format, kan vi också inkludera dagen i veckan med hjälp av bokstaven "l" och månadens fullständiga namn med "F". Vi kan också lägga till "S" för att få dagar som 1st, 2nd, 3rd istället för bara siffrorna.

## Deep Dive

När det gäller att konvertera datum till strängar finns det olika format och alternativ som kan anpassas efter användarens behov. Till exempel kan man inkludera tider, tidszoner och andra inställningar för att få ut en ännu mer specifik sträng med datumet.

En annan viktig aspekt att tänka på är inputdatatet. Om vi läser in ett datum från en användare, är det viktigt att säkerställa att det är i ett giltigt format och konvertera det till ett standardformat innan vi använder `date_format()` funktionen.

## Se även

- [PHP Date Formats](https://www.php.net/manual/en/datetime.format.php)
- [PHP Date & Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [PHP Date and Time tutorials](https://www.tutorialspoint.com/php/php_date_time.htm)