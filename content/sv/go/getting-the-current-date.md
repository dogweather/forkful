---
title:                "Att få den aktuella datumen"
html_title:           "Go: Att få den aktuella datumen"
simple_title:         "Att få den aktuella datumen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datumet är en vanlig funktion inom programmering, eftersom den ger programmet möjlighet att visa eller använda den aktuella tiden. Det kan vara användbart för att hålla koll på olika aktiviteter eller för att skapa tidsstämplar.

## Hur man gör det

För att få den aktuella datumet använder man Go:s inbyggda paket "time", som ger tillgång till olika funktioner för att hantera datum och tid.

Först måste vi importera paketet "time" i vår kod:

```Go
import "time"
```

Sedan kan vi använda funktionen "Now" för att få den aktuella tiden i form av ett "Time" objekt:

```Go
currentDate := time.Now()
```

Om vi vill ha datumet i ett specifikt format, till exempel "dd-mm-yyyy", kan vi använda "Format" funktionen:

```Go
formattedDate := currentDate.Format("02-01-2006")
```

Här använde vi ett speciellt datumlayout som Go:s "Format" funktion följer, där "02-01-2006" motsvarar "dd-mm-yyyy". Du kan använda olika kombinationer av siffror och bokstäver för att få det datumformat du föredrar.

## Djupdykning

Go:s "time" paket använder sig av tidszoner för att hantera datum och tid. Standardtidszonen är UTC, men man kan också definiera en specifik tidszon med hjälp av "LoadLocation" funktionen:

```Go
specificTimezone, err := time.LoadLocation("Europe/Stockholm")
```

Man kan sedan använda "In" funktionen för att konvertera den aktuella tiden till den specifika tidszonen:

```Go
specificTime := time.Now().In(specificTimezone)
```

Genom att använda "Now" funktionen med en specifik tidszon kan man också få den aktuella tiden för olika delar av världen.

## Se även

- [Go time paket](https://golang.org/pkg/time/)
- [Go tidlayout referens](https://golang.org/src/time/format.go?s=10064:10105#L340)
- [Dokumentation för time.Now() funktionen](https://golang.org/pkg/time/#Now)