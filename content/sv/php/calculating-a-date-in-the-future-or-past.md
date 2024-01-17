---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "PHP: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att räkna ut datum i framtiden eller det förflutna är en vanlig uppgift för programmerare. Det handlar om att ange ett startdatum och sedan beräkna ett nytt datum baserat på en given tidsperiod, till exempel att lägga till eller dra av antal dagar, veckor eller månader. Detta kan vara användbart för att planera framtidshändelser eller för att spåra tidsbaserade aktiviteter.

## Hur gör man:
Kodexempel och resultat visas nedan för att demonstrera hur man beräknar datumet för fem dagar från idag i PHP och sedan för två veckor tillbaka från idag.

```PHP 
<?php
$startDate = date('Y-m-d');
// räkna ut datum för fem dagar sedan
$futureDate = date('Y-m-d', strtotime('+5 days'));
echo "Nuvarande datum: ".$startDate."<br>";
echo "Datum för 5 dagar från nu: ".$futureDate;
?> 

//Output:
//Nuvarande datum: 2019-11-20
//Datum för 5 dagar från nu: 2019-11-25```

```PHP 
<?php
$startDate = date('Y-m-d');
// räkna ut datum för två veckor sedan
$pastDate = date('Y-m-d', strtotime('-2 weeks'));
echo "Nuvarande datum: ".$startDate."<br>";
echo "Datum för 2 veckor tillbaka: ".$pastDate;
?> 

//Output:
//Nuvarande datum: 2019-11-20
//Datum för 2 veckor tillbaka: 2019-11-06```

## Djupdykning:
Beräkning av datum har varit en viktig del av programmering sedan tidiga dagar, speciellt i system för datum- och tidsstyrning. Idag finns det också flera alternativa sätt att utföra dessa beräkningar, såsom att använda olika PHP-funktioner (t.ex. mktime() eller strtotime()) eller att använda bibliotek som Carbon. Implementeringen av denna beräkning kan variera beroende på det specifika språket eller ramverket som används.

## Se även:
För mer information och detaljerad dokumentation om date-funktionen i PHP, besök följande länkar:
- https://www.php.net/manual/en/function.date.php
- https://www.php.net/manual/en/datetime.formats.php