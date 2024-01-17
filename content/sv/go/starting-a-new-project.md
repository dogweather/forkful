---
title:                "Att börja ett nytt projekt"
html_title:           "Go: Att börja ett nytt projekt"
simple_title:         "Att börja ett nytt projekt"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi börjar ett nytt projekt, skapar vi en grund för en ny programvara. Det kan vara allt från enkla verktyg till mer komplexa appar och system. Vi gör detta för att lösa ett problem eller behov, utforska en idé eller lära oss nya saker.

## Så här gör du:
Låt oss säga att du vill starta ett nytt projekt med Go. Först måste du ha Go installerat på din dator. Sedan kan du följa dessa steg för att skapa och köra ditt projekt:

```Go
// Skapa en ny mapp för ditt projekt
mkdir mitt_projekt

// Gå in i mappen
cd mitt_projekt

// Skapa en ny Go-fil
touch main.go

// Öppna filen med din favorit texteditor
nano main.go

// Skriv lite kod för att prova
package main

import "fmt"

func main () {
    fmt.Println("Hej världen!")
}

// Spara och stäng filen

// Kör filen med Go-kommandot
go run main.go

// Du bör se "Hej världen!" i terminalen
```

## Djupdykning:
Go, som utvecklades av Google, släpptes 2009 som ett open-source språk. Det är ett språk som fokuserar på att vara enkelt, effektivt och pålitligt. Det finns andra populära programmeringsspråk som Python eller Java, men många väljer Go för sina lyhörda samarbetar och snabbt växande community.

## Se även:
För mer information om att starta ett nytt projekt i Go, kolla in dessa resurser:

- [Officiell Go dokumentation](https://golang.org/doc/)
- [A Tour of Go](https://tour.golang.org/welcome/1)
- [Go community på Reddit](https://www.reddit.com/r/golang/)