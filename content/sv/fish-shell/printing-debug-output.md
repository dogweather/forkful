---
title:                "Utskrift av felsökningsutdata"
html_title:           "Fish Shell: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utgång är ett vanligt sätt för programmerare att felsöka och hitta fel i sitt kod. Genom att skriva ut värden och variabler i olika delar av programmet kan man få en bättre förståelse för hur koden körs och var eventuella problem uppstår.

## Så här gör du

För att skriva ut debug-utgång i Fish Shell kan du använda kommandot `echo`. Detta kommando skriver ut en sträng som du anger till standardutgången. I följande exempel skriver vi ut värdet av variabeln `num`:
```
Fish Shell
set num 5
echo "Värdet av num är $num"
```

Output: 
```
Värdet av num är 5
```

För att enkelt se skillnaden mellan olika värden kan du också använda `printf` kommandot. Detta kommando tillåter dig att formatera utmatningen enligt specifika instruktioner. I följande exempel skriver vi ut två variabler i olika former:
```
Fish Shell
set num 5
set text "Det här är en text"
printf "Värdet av num är %d. %s\n" $num $text
```

Output:
```
Värdet av num är 5. Det här är en text
```

## Djupdykning

En annan användbar teknik för att skriva ut debug-utgång är att använda `status` kommandot. Detta kommando låter dig se statuskoden för det senaste kommandot som kördes. Detta kan vara särskilt användbart när du vill kontrollera om ett visst kommando har kört korrekt eller inte.

```
Fish Shell
status
```

Output:
```
0
```

Om kommandot kördes utan några problem kommer du att se `0` som resultat. Om det däremot uppstod problem kommer du att se en annan siffra som representerar en felkod.

## Se även

- [Officiell Fish Shell-dokumentation](https://fishshell.com/docs/current/)
- [How to Debug in Fish Shell](https://medium.com/@flashpoint.eth/how-to-debug-in-fish-shell-3afa8b5d6b)
- [Fish Shell Tips and Tricks](https://medium.com/@rafaelcalsaverini/fish-shell-tips-and-tricks-94d1bb66fe3a)