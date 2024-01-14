---
title:                "Bash: Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

### Varför
Att kunna räkna ut ett datum i framtiden eller förfluten tid kan vara användbart i många situationer. Det kan hjälpa dig att planera framtida händelser eller att spåra tillbaka händelser som redan har inträffat. Genom att lära dig hur man gör detta med hjälp av Bash programmering kan du enkelt utföra dessa beräkningar direkt från din terminal.

### Hur man gör det
För att räkna ut ett datum i framtiden eller förfluten tid, behöver du använda kommandot ```date``` i Bash. Detta kommando visar det aktuella datumet och tiden, men genom att lägga till vissa parametrar kan vi få det att beräkna ett framtida eller förflutet datum åt oss.

För att beräkna ett datum i framtiden kan vi använda parametern ```+``` följt av antalet dagar vi vill lägga till. Till exempel, om vi vill veta vilket datum det är om 7 dagar, kan vi använda följande kommando:

```Bash
date +7 days
```

Detta kommer att ge oss resultatet i formatet "YYYY-MM-DD". Om vi vill ha ett annat format, som t.ex. "DD/MM/YYYY", kan vi använda kommandot ```+``` igen följt av formatet vi vill ha, till exempel:

```Bash
date +"%d/%m/%Y" +7 days
```

På samma sätt kan vi beräkna ett datum i förfluten tid genom att använda parametern ```-``` istället för ```+```. Till exempel, om vi vill veta vilket datum det var för 2 veckor sedan, kan vi använda följande kommando:

```Bash
date -2 weeks
```

### Djupdykning
Datumberäkningar kan bli mer komplicerade när det kommer till månader och år. I dessa fall måste vi ta hänsyn till antalet dagar i varje månad och eventuella skottdagar. Detta kan göras med hjälp av Bashs inbyggda funktioner för datumberäkningar, som ```date --date```.

Detta kommando låter oss ange ett datum i ett visst format och sedan beräkna ett annat datum baserat på det. Till exempel, om vi vill räkna ut vilket datum det är om 1 månad och 3 dagar, kan vi använda följande kommando:

```Bash
date --date="2021-05-15 + 1 month 3 days"
```

Detta kommer att ge oss resultatet "2021-06-18". Genom att använda Bashs inbyggda funktioner för datumberäkningar kan vi ta hänsyn till alla olika faktorer som kan påverka resultatet.

### Se även
- [Linux Bash Guide](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Programming Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash Date Man Page](https://man7.org/linux/man-pages/man1/date.1.html)