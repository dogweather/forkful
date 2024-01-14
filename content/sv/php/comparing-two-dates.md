---
title:    "PHP: Jämföra två datum"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

#Varför

Att jämföra två datum är ett vanligt problem inom PHP-programmering och är viktigt för att kunna hantera och manipulera tidsdata effektivt. Genom att kunna jämföra två datum kan du till exempel sortera händelser i tidsordning eller visa information baserat på ett specifikt datum.

##Så här gör du

För att jämföra två datum i PHP kan du använda dig av funktionen `strtotime ()` som konverterar en sträng till en unix timestamp. Genom att göra detta kan du sedan enkelt jämföra två timestamps med varandra. Här är ett enkelt exempel på hur du kan jämföra två datum:

```PHP
$date1 = strtotime("2020-02-01");
$date2 = strtotime("2020-03-01");

if ($date1 > $date2) {
  echo "Datum 1 är senare än datum 2";
} elseif ($date1 < $date2) {
  echo "Datum 1 är tidigare än datum 2";
} else {
  echo "Datum 1 är samma som datum 2";
}

//Output: Datum 1 är tidigare än datum 2
```

I koden ovan konverterar `strtotime()` funktionen `2020-02-01` och `2020-03-01` till timestamps och jämför sedan dem med hjälp av en `if`-sats.

##Djupdykning

Förutom att endast jämföra två datum kan du också utföra olika typer av manipulationer och beräkningar med hjälp av timestamps. Till exempel kan du lägga till eller subtrahera dagar, timmar eller minuter från ett datum. Här är ett exempel på hur du kan lägga till 7 dagar på ett datum:

```PHP
$date = strtotime("2020-02-01");
$modified_date = strtotime("+7 days", $date);

echo date("Y-m-d", $modified_date);

//Output: 2020-02-08
```

Genom att lägga till 7 dagar får vi ett nytt datum som är en vecka senare. Du kan använda samma metod för att subtrahera eller utföra andra typer av manipulationer.

##Se även

* [PHP Date and Time functions](https://www.php.net/manual/en/ref.datetime.php)
* [The strtotime() function](https://www.php.net/manual/en/function.strtotime.php)
* [Sort dates in PHP](https://www.geeksforgeeks.org/sort-dates-in-php/)