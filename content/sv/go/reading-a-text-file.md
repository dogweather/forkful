---
title:                "Läsa en textfil"
date:                  2024-01-20T17:54:32.067239-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa en textfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil innebär att programmässigt öppna och hämta innehållet från en fil i textformat. Programmerare gör det för att till exempel bearbeta data, ladda konfigurationer eller bara visa text för användaren.

## Hur gör man:

```Go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("example.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
```
Exempelutdata:
```
Hej! Där är texten du läste in!
En annan rad med text.
```

## Fördjupning:

Att läsa textfiler i Go har sina rötter i de äldre koncepten om filhantering i programmering. Go språket är dock designat för moderna system, med ett standardbibliotek fyllt med verktyg för just det.

Alternativ till `bufio.Scanner`, som används för att läsa rader, är `ioutil.ReadFile()` (användbart för små filer då det läser hela filen på en gång) eller `os.ReadFile()` (i senare versioner av Go). För stora filer kan `bufio.Reader` användas då den har mer kontroll över buffering.

Djupare in i koden, filhantering i Go hanterar bytes och runes för att stödja olika teckenkodningar, viktigt i en globaliserad värld. 

## Se även:

- Go by Example: Reading Files [https://gobyexample.com/reading-files](https://gobyexample.com/reading-files)
- Go Docs: Package os [https://pkg.go.dev/os](https://pkg.go.dev/os)
- Go Docs: Package bufio [https://pkg.go.dev/bufio](https://pkg.go.dev/bufio)
- Go Blog: Defer, Panic, and Recover [https://blog.golang.org/defer-panic-and-recover](https://blog.golang.org/defer-panic-and-recover)
- Effective Go: Reading and Writing Files [https://golang.org/doc/effective_go#reading](https://golang.org/doc/effective_go#reading)
