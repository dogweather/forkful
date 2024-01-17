---
title:                "Jämföra två datum"
html_title:           "Go: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att jämföra två datum är en vanlig uppgift för programmerare. Genom att kunna jämföra datum kan vi kontrollera om ett datum är före eller efter ett annat, eller om de är samma dag. Detta är användbart när man hanterar tid och datum i program.

# Så här gör du:
För att jämföra två datum i Go kan du använda funktionen `Equal` från paketet "time". För att använda denna funktion måste du först konvertera dina datum till "Time" typen. Du kan sedan använda `Equal` för att jämföra de två datum och få ett boolskt värde som resultat.

```
Go date1 := time.Date(2021, time.August, 19, 0, 0, 0, 0, time.UTC)
Go date2 := time.Date(2021, time.August, 20, 0, 0, 0, 0, time.UTC)

Go result := date1.Equal(date2)

// Resultatet blir false eftersom date2 är en dag efter date1.
```

# Djupdykning:
Att hantera tid kan vara en utmaning, särskilt när det kommer till att hantera tidszoner och sommartid. Go har dock ett välfungerande paket för tidshantering, "time", som tar hand om dessa utmaningar på ett enkelt sätt.

Som en alternativ metod för att jämföra datum kan du också använda `Before` och `After` funktionerna från "time" paketet. Dessa funktioner returnerar ett boolskt värde baserat på om ett datum är före eller efter det andra.

När du jämför två datum bör du också ta hänsyn till tidszoner och sommartid för att få korrekta resultat.

# Se även:
Läs mer om "time" paketet och dess funktioner på Go's officiella dokumentationssida: https://golang.org/pkg/time/

För en mer ingående förklaring om tidszoner och sommartid, se denna artikel på Medium: https://medium.com/@arjunpatel/time-zones-in-go-489f52e05228