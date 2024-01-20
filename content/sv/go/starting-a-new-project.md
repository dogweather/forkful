---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Starta ett nytt projekt handlar om att bygga en ny programvara från grunden. Programmerare gör det för att skapa anpassade lösningar på unika problem.

## Hur man:

Att skapa ett nytt projekt i Go kräver några grundläggande steg. Först måste vi installera Go på vårt system. Sedan kan vi skapa en mapp för vårt nya projekt och skriva en grundläggande "helloworld.go" fil.

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hej Värld!")
}
```

För att köra vår kod, navigerar vi till vår projektmapp i terminalen och skriver `go run helloworld.go`. Vi bör få output som detta:

```Console
Hej Värld!
```

## Djupdykning

Go, eller GoLang som det ofta kallas, skapades på Google 2007 för att lösa problem relaterade till storskalig systemutveckling. Några alternativ till Go inkluderar språk som Python och JavaScript. Go skiljer sig dock ut för dess effektiva minneshantering, enkelhet och snabba exekveringstid. När vi startar ett nytt projekt, skapar Go en binärfil som kan köras oberoende, vilket gör distributionen av applikationen mycket lättare.

## Se även:

- Go Programmeringsokumentation: https://golang.org/doc/
- Go Programmeringsblogg: https://blog.golang.org/
- Go på GitHub: https://github.com/golang/go
- Go Playground: https://play.golang.org/