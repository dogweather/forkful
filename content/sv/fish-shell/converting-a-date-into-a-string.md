---
title:    "Fish Shell: Omvandla ett datum till en sträng"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är ett vanligt problem som kan uppstå när man jobbar med datorprogrammering. En vanlig anledning kan vara att man behöver skapa en rapport eller utskrift där datumet ska visas på ett särskilt sätt. I detta blogginlägg ska vi titta på hur man gör detta i Fish Shell och varför det kan vara en nyttig färdighet att ha i sin programmering.

## Hur man gör det

För att konvertera ett datum till en sträng i Fish Shell kan man använda kommandot `date -f`. Detta kommando tar emot ett datum och en formateringssträng som argument och ger tillbaka datumet som en sträng i det önskade formatet.

```Fish Shell
date -f "%A, %B %d, %Y" "2021-07-15"
```

Detta kommer att ge följande utmatning:

`Thursday, July 15, 2021`

Det finns en mängd olika formateringsalternativ som man kan använda, beroende på hur man vill att datumet ska visas. Här är några exempel:

```Fish Shell
date -f "%m/%d/%Y" "2021-07-15"
```

`07/15/2021`

```Fish Shell
date -f "%Y-%m-%d" "2021-07-15"
```

`2021-07-15`

## Djupdykning

Som vi nämnde tidigare finns det många olika formateringsalternativ som man kan använda när man konverterar ett datum till en sträng. En utmärkt resurs för att lära sig mer om vilka alternativ som finns tillgängliga är Fish Shells officiella dokumentation för `date` kommandot.

Några saker att tänka på när man arbetar med datum och strängar är att vissa tecken kan ha en särskild betydelse och måste därför backslashas för att visas rätt. Exempelvis representerar `%Y` det fullständiga året med fyra siffror, medan `%y` endast representerar de två sista siffrorna. För att undvika förvirring kan det vara bra att testa olika formateringssträngar med ett datum och se vilken utmatning man får.

## Se även

- [Fish Shell dokumentation för date kommandot](https://fishshell.com/docs/current/cmds/date.html)