---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---

# Att Läsa Kommandoradsargument i Go: En Snabbgenomgång

---

## Vad & Varför?

Kommandoradsargument är ett enkelt sätt för ett program att ta emot data när det startar. Det gör din kod mer flexibel och anpassningsbar efter olika scenarion.

---

## Såhär Gör Man:

Här är ett exempel på hur du kan hantera kommandoradsargument i Go:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	argList := os.Args
	for index, val := range argList {
		fmt.Printf("Index: %d, Value: %s\n", index, val)
	}
}
```

Om du kör detta program med kommandot `go run main.go Hej Världen`, så kommer du att se:

```Go
Index: 0, Value: main
Index: 1, Value: Hej
Index: 2, Value: Världen
```

---

## Djupdykning

Historiskt sett kommer användningen av kommandoradsargument från tidig Unix-stil terminalbaserade system, där det var norm att välja programbeteenden med flaggor och argument.

Som alternativ till `os.Args` finns det även mer omfattande bibliotek som `flag` eller tredjepartsbibliotek som `cobra`, om du behöver en mer detaljerad hantering av kommandoradsargument.

Något att komma ihåg är att `os.Args` innehåller även programnamsfilen som det första argumentet (`os.Args[0]`). Riktiga argument startar från index 1.

---

## Se Även

- För mer detaljerad information, se officiella [Go's dokumentationen om os.Args](https://golang.org/pkg/os/#Args).
- För mer avancerade användningsfall, rekommenderar jag att du undersöker biblioteken [`flag`](https://golang.org/pkg/flag/) och [`cobra`](https://github.com/spf13/cobra).

Lycka till och happy coding!