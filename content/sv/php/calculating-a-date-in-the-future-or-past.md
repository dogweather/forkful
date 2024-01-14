---
title:    "PHP: Beräkning av ett datum i framtiden eller förflutet."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller förflutet är en användbar och grundläggande funktion i många PHP-program. Det är särskilt användbart i applikationer som hanterar bokningar, tidigare händelser eller tidsbaserade påminnelser.

## Så här gör du

För att beräkna ett datum i framtiden eller förflutet behöver du använda PHP-funktionen `date()` i kombination med `strtotime()`. Låt oss titta på ett enkelt exempel:

```PHP
$today = date("Y-m-d"); // Här får vi dagens datum i formatet ÅÅÅÅ-MM-DD
$future_date = strtotime("+1 week"); // Detta beräknar datumet för exakt en vecka framåt i tiden
echo date("Y-m-d", $future_date); // Skriver ut det nya datumet, som nu är en vecka framåt i tiden
```

I det här exemplet skrytt vi ut 2020-07-21 (om dagens datum är 2020-07-14), vilket är en vecka efter det aktuella datumet. Om du vill beräkna ett datum i förflutet istället för i framtiden kan du helt enkelt ändra ordningen på funktionerna:

```PHP
$past_date = strtotime("-1 month"); // Detta beräknar datumet för exakt en månad tillbaka i tiden
echo date("Y-m-d", $past_date); // Skriver ut det nya datumet, som nu är en månad bakåt i tiden
```

Det är också möjligt att använda andra tidsenheter som dagar, år eller till och med specifika tider som klockslag. Du kan läsa mer om hur du använder `strtotime()` på [PHPs dokumentationssida](https://www.php.net/manual/en/function.strtotime.php).

## Djupdykning

Förutom att enkelt kunna beräkna datum i framtiden eller förflutet, kan du också formatera datumet på olika sätt med hjälp av `date()`-funktionen. Till exempel kan du skriva ut namnet på veckodagen eller månaden, eller välja att endast skriva ut en del av datumet (som året eller månaden).

Det är också viktigt att ta hänsyn till tidszoner när du hanterar datum i dina PHP-program. Det kan finnas tillfällen då du behöver justera dato-med så att de återspeglar rätt tidszon för din användare. PHP har också en inbyggd funktion för att hantera tidszoner, `date_default_timezone_set()`, som du kan läsa mer om på [PHPs dokumentationssida](https://www.php.net/manual/en/function.date-default-timezone-set.php).

## Se även

- [PHPs dokumentation om date()](https://www.php.net/manual/en/function.date.php)
- [Mer om strtotime() hos PHP](https://www.php.net/manual/en/function.strtotime.php)
- [Guide till att hantera tidszoner i PHP](https://www.php.net/manual/en/datetime.configuration.php)