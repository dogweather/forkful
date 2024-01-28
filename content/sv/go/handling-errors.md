---
title:                "Hantering av fel"
date:                  2024-01-26T00:52:57.430867-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/handling-errors.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Felhantering i Go handlar om att smidigt fånga upp och svara på körtidsproblem. Vi gör det för att förhindra krascher och försäkra oss om att våra program uppträder förutsägbart, även när saker och ting går snett.

## Hur man gör:

Go använder explicit felhantering. Det innebär att du kommer att kontrollera om en funktion returnerar ett fel varje gång du anropar den. Inga undantag. Så här ser det ut:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Hoppsan:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Låtsas att något gick fel
	return fmt.Errorf("något gick fel")
}
```

Kör detta, och du får:

```
Hoppsan: något gick fel
```

Men vad händer om det lyckas?

```Go
func doSomething() error {
	// Allt bra den här gången
	return nil
}
```

Ingen utskrift. Coolt, inga nyheter är goda nyheter.

## Fördjupning:

I Go har felhantering varit en kontroversiell punkt. Sedan starten beslutade Go sig mot undantag till förmån för en mer explicit metod, vilket vissa utvecklare älskar för dess enkelhet och andra hittar verbose. Den inbyggda `error`-typen är ett gränssnitt. Alla typer med en `Error() string`-metod uppfyller det. Detta går hand i hand med Gots ethos om enkelhet och explicititet.

Alternativ? Det finns duon `panic` och `recover`, men de är för exceptionella fall (ordvits avsedd) när programmet inte kan fortsätta. Tänk på `panic` som utkastningsknappen du trycker på när du vet att det inte finns någon återvändo. Använd det sparsamt.

När det gäller mainstream felhantering introducerade Go 1.13 felinkapsling, vilket gjorde det enklare att förstå "felskedjan" med funktioner som `errors.Is()` och `errors.As()`.

## Se även:

För allt om felhantering i Go:

- The Go Blog om felhantering: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – avsnittet om felhantering: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13-dokumentation om felinkapsling: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheneys inlägg om strategier för felhantering: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
