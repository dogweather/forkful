---
title:                "Go: Beräkning av ett datum i framtiden eller passerat"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att räkna ut ett datum i framtiden eller förflutna kan vara användbart för att planera evenemang eller för att hålla koll på födelsedagar. Med Go-programmeringsspråket är det enkelt att beräkna datum med hjälp av inbyggda funktioner.

## Hur man gör

För att beräkna ett datum i framtiden eller förflutna i Go använder man sig av tidenhetspaketet "time". Först måste man importera paketet genom att skriva ```Go
import "time"``` i början av programmet.

För att beräkna ett datum i framtiden kan man använda funktionen ```Go
AddDate()``` tillsammans med dagar, månader och år som vill lägga till. Till exempel, om vi vill beräkna datumet tre månader framåt kan vi använda följande kod:
```Go
futureDate := time.Now().AddDate(0, 3, 0)
fmt.Println(futureDate) // exempel output: 2022-02-19 22:07:00.2803962 +0100 CET m=+91.996585201
```

För att beräkna ett datum i förflutna använder man samma funktion, men med ett negativt antal dagar, månader eller år. Till exempel, om vi vill beräkna datumet ett år tillbaka kan vi använda följande kod:
```Go
pastDate := time.Now().AddDate(-1, 0, 0)
fmt.Println(pastDate) // exempel output: 2020-11-19 22:08:54.2793962 +0100 CET m=+86399.995185801
```

## Djupdykning

Go erbjuder även andra inbyggda funktioner för datumberäkning, såsom ```Go
Date()``` och ```Go
Parse()```. Dessutom finns det flera externa paket som erbjuder mer avancerade funktioner för datumhantering i Go.

En viktig aspekt att tänka på när man beräknar datum i Go är att Go använder sig av den gregorianska kalendern, vilket innebär att funktionerna kan bete sig annorlunda med andra tidsskalor eller kalendrar. Det är viktigt att ta hänsyn till detta vid användning av dessa funktioner.

## Se även

- [Go dokumentation för tidenhetspaketet](https://golang.org/pkg/time/)
- [Paketet "dateparse" för mer avancerad datumhantering i Go](https://github.com/araddon/dateparse)
- [Alternativa kalenderpaket för Go](https://github.com/dovbysh/awesome-go-by-example-swe#dateTime)