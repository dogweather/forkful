---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva ut till `stderr` signerar fel eller viktiga meddelanden. Det hjälper att snabbt separera dessa från normal `stdout` output.

## How to:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	errMsg := "Hittade ett fel!"
	if _, err := os.Stderr.WriteString(errMsg); err != nil {
		panic(err)
	}
}
```
Output i terminalen är felmeddelandet men inte via standard utdatan.

## Deep Dive
`stderr`, från början en del av Unix, används för att skilja normal data från felmeddelanden. Alternativ inkluderar loggning till filer eller externa system. I Go, använder `os.Stderr` en global `*File` variabel som refererar till standard error stream.

## See Also
- Go dokumentationen om I/O: https://pkg.go.dev/io
- `log` paketet i Go för avancerad loggning: https://pkg.go.dev/log
- Unix's standard streams: https://en.wikipedia.org/wiki/Standard_streams
