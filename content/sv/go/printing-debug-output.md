---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:52:53.994329-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Utskrift av felsökningsdata är helt enkelt att visa temporär info som hjälper utvecklare att förstå vad som händer i koden. Vi gör detta för att snabbt hitta och rätta buggar under utvecklingsprocessen.

## Hur gör man?:
I Go kan du använda paketen `fmt` och `log` för att skriva ut felsökningsinformation. Här är ett enkelt exempel:

```Go
package main

import (
	"fmt"
	"log"
)

func main() {
	// Använd fmt för att skriva ut till standard output
	fmt.Println("Här är en felsökning via fmt")

	// Konfigurera loggaren
	log.SetFlags(log.Ldate | log.Lmicroseconds | log.Llongfile)
	log.Println("Här är en felsökning via log")
}
```

Sample output:

```
Här är en felsökning via fmt
2023/04/01 15:04:05.123456 /full/path/to/your/file.go:12: Här är en felsökning via log
```

## Djupdykning:
Felsökningsutskrifter är en gammal teknik som funnits sedan programmeringens barndom. Alternativa tillvägagångssätt inkluderar mer avancerad loggning, programvaror för att spåra exekvering eller debuggers som `delve` i Go. Implementation i Go är designad för att vara enkel och rakt på sak; `fmt` för att skriva ut och `log` för mer detaljerad information, inklusive tidsstämpel och filkälla.

## Se även:
- Go's fmt package: https://pkg.go.dev/fmt
- Go's log package: https://pkg.go.dev/log
- Delve, a debugger for the Go programming language: https://github.com/go-delve/delve

Observera att länkarna leder till engelskspråkiga resurser då Go's officiella dokumentation och verktyg ofta är på engelska.
