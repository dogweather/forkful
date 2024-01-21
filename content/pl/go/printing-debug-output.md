---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-01-20T17:52:38.605262-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Drukowanie informacji diagnostycznych (debug output) to wyświetlanie danych pomocnych przy łapaniu błędów i zrozumieniu, co się dzieje w programie. Robimy to, by szybko znaleźć źródło problemu i naprawić kod bez strzelania w ciemno.

## Jak to zrobić:
```Go
package main

import (
	"fmt"
	"log"
)

func main() {
	// Proste wyświetlanie komunikatu
	fmt.Println("Hello, debug!")

	// Formatowanie stringów
	name := "Go Programmer"
	fmt.Printf("Good to see you, %s!\n", name)

	// Użycie standardowego loggera do debugowania
	debug := false
	if debug {
		log.Printf("Debug info: Memory address of 'name' variable: %p", &name)
	}
}
```

Wynik:
```
Hello, debug!
Good to see you, Go Programmer!
```

## Pogłębione informacje:
Kiedyś, przed logerami i zintegrowanymi środowiskami programistycznymi (IDE), debugowanie było dużo trudniejsze – często bazowało na prostym wydruku na ekran lub do pliku. W Go, poza standardowym pakietem `fmt`, warto znać pakiet `log`, który oferuje więcej możliwości, jak logowanie z poziomem ważności czy zapis do pliku. Implementacja logera w Go zapewnia też mechanizmy do uniknięcia wypisywania debugu na produkcji dzięki flagom i poziomom logowania.

## Zobacz także:
- Dokumentacja fmt: https://pkg.go.dev/fmt
- Dokumentacja log: https://pkg.go.dev/log
- Artykuł o efektywnym debugowaniu w Go: https://blog.golang.org/debugging-what-you-deploy