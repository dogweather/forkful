---
title:                "Go: Skriva till standardfel"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standardfel är en viktig del av Go-programmering eftersom det ger en enkel och användbar metod för att hantera fel och rapportera status till användaren. Det är också ett bra sätt att hålla koden mer ren och lättläst genom att skilja ut felhanteringskoden från huvudflödet.

## Hur man gör det
Det finns flera sätt att skriva till standardfel i Go. Ett enkelt sätt är att använda funktionen `fmt.Fprintln()` där den första parameter är `os.Stderr` för att skriva till standardfel. Här är ett exempel:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	numbers := []int{1, 2, 3, 4, 5}
	index := 6
	if index >= len(numbers) {
		fmt.Fprintln(os.Stderr, "Index utanför omfånget")
		return
	}
	fmt.Println(numbers[index])
}
```

Output:
```
Index utanför omfånget
```

I det här exemplet försöker vi få tillgång till ett element utanför listans omfång, vilket kommer att orsaka ett fel. Genom att använda `fmt.Fprintln()` och `os.Stderr` i felhanteringskoden, kommer felmeddelandet att skrivas ut på standardfelströmmen.

## Djupdykning
Att använda `fmt.Fprintln()` är enkelt och effektivt för att skriva till standardfel, men det finns andra metoder som kan användas. Ett exempel är att använda `log`-paketet som erbjuder mer avancerade funktioner för att hantera fel och loggning. Det finns även möjlighet att skapa en egen typ av fel och använda `fmt.Fprintf()` för att formatera felmeddelandet på ett mer flexibelt sätt.

## Se även
- [fmt-paketet på Go-docs](https://golang.org/pkg/fmt/)
- [log-paketet på Go-docs](https://golang.org/pkg/log/)