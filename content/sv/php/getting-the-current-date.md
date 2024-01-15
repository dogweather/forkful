---
title:                "Att få aktuellt datum"
html_title:           "PHP: Att få aktuellt datum"
simple_title:         "Att få aktuellt datum"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella datumet är en viktig färdighet i många programmeringsprojekt. Det kan användas för att skapa loggar, schemalägga uppgifter eller visa datumet till användaren.

## Hur man gör det

Det finns två sätt att få den aktuella datumet i PHP: genom att använda den inbyggda funktionen "date()" eller genom att använda den inbyggda klassen "DateTime". Låt oss undersöka båda metoderna nedan.

### Använda date()

```PHP
$currentDate = date("Y-m-d"); // Formatet för datumet är valfritt
echo $currentDate; // Output: 2021-11-23
```

I exemplet ovan används funktionen "date()" med en parameter som anger önskat format för datumet. Det finns olika formatalternativ att välja mellan, såsom "d/m/Y" för att visa datumet i formatet dag/månad/år eller "h:i:s A" för att visa tiden i formatet timme:minut:sekund am/pm.

### Använda DateTime-klassen

```PHP
$currentDate = new DateTime(); // Skapar ett nytt DateTime-objekt
echo $currentDate->format("Y-m-d"); // Output: 2021-11-23
```

Här skapas ett nytt objekt av klassen "DateTime" som har flera inbyggda metoder för att formatera och manipulera datumet. Genom att använda metoden "format()" med önskat format som parameter kan vi få den aktuella datumet i önskad format.

## Djupdykning

Båda metoderna som nämnts ovan kan också användas för att få mer detaljerad information om datumet, såsom dag i veckan, veckonummer och tidszon. För att få reda på dag i veckan kan vi använda "l" och för veckonummer "W" i formatet.

```PHP
$currentDate = new DateTime();
echo $currentDate->format("l"); // Output: Tuesday
echo $currentDate->format("W"); // Output: 47
```

Genom att använda metoden "getTimezone()" kan vi också få information om den aktuella tidszonen.

## Se också

- [PHP date-funktionen](https://www.php.net/manual/en/function.date.php)
- [PHP DateTime-klassen](https://www.php.net/manual/en/class.datetime.php)