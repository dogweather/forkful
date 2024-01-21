---
title:                "Läsa in kommandoradsargument"
date:                  2024-01-20T17:55:55.330178-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument i Go handlar om att fånga texten som användaren skriver när de kör ditt program. Varför? För att låta användaren påverka programmets beteende utan att ändra koden.

## Så här gör du:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	arguments := os.Args[1:] // Ignorerar första argumentet (programmets sökväg)
	for _, arg := range arguments {
		fmt.Println(arg)
	}
}
```
Kör programmet så här: `go run yourprogram.go arg1 arg2`
Förväntad output:
```
arg1
arg2
```

## Djupdykning
Historiskt sett härstammar kommandoradsargument från tidiga dagar av datorprogrammering där grafiska gränssnitt var ovanliga. Andra språk använder liknande konventioner, men i Go är `os.Args` en slice som innehåller alla kommandoradsargument. Den första posten är programmets sökväg, därefter följer de faktiska argumenten. Alternativ till `os.Args` inkluderar flaggpaket (`flag`) för mer komplexa behov, som att definiera och processa kommandoradsflaggor.

## Se även:
- Go dokumentation för `os.Args`: https://pkg.go.dev/os#Args
- Go dokumentation för flaggpaketet: https://pkg.go.dev/flag
- Artikel om att skriva kommandoradsverktyg i Go: https://go.dev/doc/cmd