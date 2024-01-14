---
title:    "PHP: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför?

Att kunna beräkna datum i framtiden eller från det förflutna är en viktig färdighet för alla som arbetar med webbutveckling. Det kan användas för att skapa dynamiska funktioner på en webbplats som behöver visa datum som är beroende av användarens ålder eller ett specifikt datum i framtiden.

## Hur man gör det

Beräkningen av datum i PHP är enkel att göra med några få rader kod. Först måste du definiera ett startdatum och sedan kan du använda PHP's `date` funktion för att räkna ut önskat datum. Se exempelkoden nedan:

```PHP
// Definiera startdatumet
$start_date = "2021-01-01";

// Beräkna datumet 6 månader framåt
$future_date = date('Y-m-d', strtotime('+6 months', strtotime($start_date)));

// Beräkna datumet 2 år bakåt
$past_date = date('Y-m-d', strtotime('-2 years', strtotime($start_date)));

// Skriv ut resultatet
echo "Datumet 6 månader fram i tiden är: " . $future_date;
echo "Datumet 2 år bakåt är: " . $past_date;
```

## Djupdykning

När man arbetar med datum i PHP finns det några saker man bör tänka på. För att undvika problem med tidszoner bör man alltid använda `strtotime` funktionen tillsammans med `date` funktionen. Det är också viktigt att se till att formatet på startdatumet och utdatumen är det samma.

För mer information och exempel på hur man kan beräkna datum med specifika tidsintervall, som veckor eller dagar, rekommenderas att läsa dokumentationen för `strtotime` och `date` funktionerna på PHP's officiella hemsida.

## Se även

- [PHP's officiella hemsida](https://www.php.net/)
- [Dokumentation för strtotime funktionen](https://www.php.net/manual/en/function.strtotime.php)
- [Dokumentation för date funktionen](https://www.php.net/manual/en/function.date.php)