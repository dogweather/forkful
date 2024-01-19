---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# Att Skriva ut Debuggutdata i Go: En Praktisk Guide
---
## Vad & Varför?
Att skriva ut debuggutdata är processen att visa viktiga data och processer som pågår i din kod. Programmerare gör det för att identifiera och fixa fel eller buggar i sin kod.

## Hur man gör:
Att skriva ut debuggutdata i Go är ganska enkelt. Du kommer mestadels att använda `fmt.Println()`, `fmt.Printf()`, eller `log package`.

Här är ett grundläggande exempel på hur du gör det:

```Go
package main

import (
	"fmt"
	"log"
)

func main() {
	message := "Detta är ett debuggmeddelande"

	fmt.Println("Fmt println:", message)

	fmt.Printf("Fmt printf: %s\n", message)

	log.Println("Log println:", message)
}
```
När du kör denna kod kommer utdata vara något liknande:

```
Fmt println: Detta är ett debuggmeddelande
Fmt printf: Detta är ett debuggmeddelande
2022/01/03 00:00:00 Log println: Detta är ett debuggmeddelande
```

## Fördjupning
Utskrift av debuggutdata har en lång historia i programmering och används allmänt för att felsöka program. I Go kan du också använda paket som `logrus` eller `zerolog` för mer avancerad loggning.

En särskild punkt att notera är att `log.Println()` automatiskt lägger till datum och tid vilket kan vara användbart för att spåra när ett visst meddelande skrivs ut.

## Se även
Relaterade resurser till detta ämne är:
1. Go's officiella dokumentation om [fmt](https://golang.org/pkg/fmt/) och [log](https://golang.org/pkg/log/) paketen
2. [Logrus GitHub repository](https://github.com/sirupsen/logrus)
3. [Zerolog GitHub repository](https://github.com/rs/zerolog)