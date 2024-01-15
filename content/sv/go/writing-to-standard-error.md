---
title:                "Skrivande till standardfel"
html_title:           "Go: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standardfel när du programmerar kan hjälpa dig att felsöka och hitta eventuella problem i ditt program. Genom att skicka felmeddelanden till standardfel istället för standardutdata, kan du enkelt skilja mellan de två och fokusera på att lösa problem.

## Hur du gör
För att skriva till standardfel i Go, behöver du använda funktionen `fmt.Fprintln()` och specificera standardfel som din destination. Här är ett exempel på en kod som skriver ut ett felmeddelande till standardfel:

```Go
fmt.Fprintln(os.Stderr, "Detta är ett felmeddelande!")
```

Detta kommer att skriva ut meddelandet "Detta är ett felmeddelande!" till standardfel. Värdet `os.Stderr` i koden ovan specificerar att standardfel är destinationen för vårt felmeddelande.

## Djupdykning
När du skriver till standardfel, är det viktigt att förstå skillnaden mellan standardfel och standardutdata. Standardutdata är den vanliga platsen där ditt program skriver ut information, medan standardfel används för fel och varningar.

En annan viktig punkt att notera är att standardfel inte ska användas för vanliga felhanteringsrutiner. Istället bör du använda paketet `log` för att logga fel i ditt program.

## Se även
- [Go Language - fmt Package](https://golang.org/pkg/fmt/)
- [Go Language - os Package](https://golang.org/pkg/os/)
- [Go Language - log Package](https://golang.org/pkg/log/)