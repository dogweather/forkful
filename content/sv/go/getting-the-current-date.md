---
title:                "Go: Att få nuvarande datum."
simple_title:         "Att få nuvarande datum."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda den aktuella datumet i sina Go-program kan vara väldigt användbart för många utvecklare. Det kan till exempel användas för att ange sökfilter, skapa loggfiler eller för att visa datumet till användare.

## Hur Man Gör

För att hämta den aktuella datumet i Go kan du använda funktionen `time.Now()` och sedan använda metoden `Format()` för att formatera det på önskat sätt.

```
Go
currentDate := time.Now()
formattedDate := currentDate.Format("2006-01-02")
fmt.Println("Idag är det", formattedDate)
```

Output: `Idag är det 2021-08-19`

I kodexemplet används layouten `2006-01-02`, vilket är ett speciellt format som används i Go för att representera datum. Den första siffran är året, den andra är månaden och den tredje är dagen. Det finns många olika möjliga layouter som kan användas, beroende på vilket format som passar bäst för ditt specifika projekt.

## Djupdykning

När vi använder funktionen `time.Now()` hämtas den aktuella tiden från systemets klocka och konverteras till en `time.Time`-strukt i Go. Den här strukturen innehåller information om årtal, månad, dag, timme, minut, sekund och nanosekund. Genom att använda metoder som `Format()` eller `AddDate()` kan vi sedan hämta och manipulera denna information på olika sätt.

Det finns också andra paket som kan vara hjälpsamma när det gäller att hantera datum och tid i Go, t.ex. `timeparse` för att parsaa datum från textsträngar eller `timezones` för att hantera tidszoner.

## Se Även

- [Go Dokumentation - Time Paketet](https://golang.org/pkg/time/)
- [Go Dokumentation - Timeparse Paketet](https://golang.org/pkg/timeparse/)
- [Go Dokumentation - Timezones Paketet](https://golang.org/pkg/timezones/)