---
title:    "Fish Shell: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna räkna ut en datum i framtiden eller förfluten tid kan vara mycket användbart i många olika situationer. Genom att lära sig hur man programmerar detta i Fish Shell kan man enkelt automatisera denna process och spara tid och ansträngning.

## Hur man gör det

För att kunna beräkna ett datum i framtiden eller förfluten tid behöver vi använda oss av de inbyggda funktionerna i Fish Shell. Här nedanför kommer vi gå igenom ett exempel på hur man kan beräkna ett datum ett visst antal dagar framåt.

```Fish Shell
set start_date (date +%Y-%m-%d)
set days_to_add 7
set future_date (cmd /possible/way/to/calculate ${start_date} +${days_to_add})
echo "Datumet " $future_date "är" $days_to_add "dagar framåt från " $start_date
```

Detta kodblock först sätter en variabel `start_date` till dagens datum med formatet YYYY-MM-DD. Sedan sätts variabeln `days_to_add` till antalet dagar som vi vill lägga till. Det sista steget är att använda en relevant kommando som låter oss beräkna datumet baserat på startdatumet och antalet dagar vi vill lägga till. I exemplet använder vi "cmd" som är en placeholder för en eventuell kommando vi behöver använda för att räkna fram datumet.

## Gräva djupare

För att verkligen förstå hur vi beräknar ett datum i framtiden eller förfluten tid behöver vi förstå hur Fish Shell behandlar datum och tidsdata. Fish Shell utnyttjar en version av musl-libc som tillåter kommandon att manipulera och beräkna datum och tid baserat på UNIX-tiden. Detta möjliggör för oss att använda Unix timestamps och konvertera dem till önskat datum och vice versa.

## Se även

* [Fish Shell dokumentation om hur man arbetar med datum och tidsdata](https://fishshell.com/docs/current/commands.html#date)
* [Enkel guide till hur man användar datum manipulation i Fish Shell](https://medium.com/@outofambit/date-manipulation-in-fish-shell-e96d6e556531)