---
title:                "Att beräkna ett datum i framtiden eller det förflutna"
html_title:           "Go: Att beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Att beräkna ett datum i framtiden eller det förflutna"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När vi pratar om att beräkna ett datum i framtiden eller i förflutna, så pratar vi om att ta en datum och sedan lägga till eller subtrahera ett visst antal dagar, veckor, månader eller år från det datumet. Detta är ett vanligt problem för programmerare, eftersom många program och applikationer behöver kunna hantera datum och tid på ett korrekt sätt.

## Så här gör man:

```Go
// Beräkna ett datum i förflutna (10 dagar tillbaka)
nu := time.Now()
förraVeckan := nu.AddDate(0, 0, -10)

// Beräkna ett datum i framtiden (2 månader framåt)
nu := time.Now()
omTvåmånader := nu.AddDate(0, 2, 0)

// Skriv ut resultaten
fmt.Println(förraVeckan.Format("2006-01-02"))
fmt.Println(omTvåmånader.Format("2006-01-02"))
```

Output:
```
2020-08-09
2020-12-07
```

## Djupdykning:

Att hantera datum och tid har varit en utmaning för programmerare sedan länge. För att lösa detta problem har många olika metoder utvecklats, till exempel att räkna antalet sekunder från ett visst datum eller att använda en kalender med förbestämda datum. I Go används funktionen `AddDate` för att beräkna ett datum i framtiden eller förflutna, men det finns också andra alternativ såsom att använda paketet `time.Duration` för att lägga till eller subtrahera en specifik tidsintervall.

## Se även:

- [Go Time Package documentation](https://golang.org/pkg/time/)
- [Go Time and Date Functions Cheat Sheet](https://yourbasic.org/golang/datetime-functions/)
- [Handling date and time in programming](https://hackernoon.com/handling-date-and-time-in-programming-8c80c2307fcb)