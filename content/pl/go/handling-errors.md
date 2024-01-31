---
title:                "Obsługa błędów"
date:                  2024-01-26T00:53:47.620005-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa błędów w Go polega na eleganckim łapaniu i reagowaniu na problemy w czasie wykonania. Robimy to, aby zapobiec awariom i upewnić się, że nasze programy działają przewidywalnie, nawet gdy coś pójdzie nie tak.

## Jak to zrobić:

Go używa jawnego obsługiwania błędów. To oznacza, że za każdym razem kiedy wywołujesz funkcję, będziesz sprawdzać, czy zwraca ona błąd. Bez wyjątków. Oto jak to wygląda:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Ups:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Udajemy, że coś poszło nie tak
	return fmt.Errorf("coś poszło nie tak")
}
```

Uruchomienie tego skutkuje:

```
Ups: coś poszło nie tak
```

Ale co jeśli zakończy się sukcesem?

```Go
func doSomething() error {
	// Tym razem wszystko dobrze
	return nil
}
```

Żadnego wyjścia. Super, brak wiadomości to dobra wiadomość.

## Dokładniej:

W Go obsługa błędów była punktem spornym. Od początku Go zdecydowało się odrzucić wyjątki na rzecz bardziej jawnej metody, którą niektórzy programiści uwielbiają za prostotę, a inni uważają za rozwlekłą. Wbudowany typ `error` jest interfejsem. Każdy typ z metodą `Error() string` go zadowala. To wpisuje się w filozofię Go dotyczącą prostoty i jasności.

Alternatywy? Jest duet `panic` i `recover`, ale są one dla wyjątkowych przypadków (i tu gra słów zamierzona), gdy program nie może kontynuować działania. Pomyśl o `panic` jak o przycisku wyrzucania, który naciskasz, gdy wiesz, że nie ma odwrotu. Używaj go oszczędnie.

Jeśli chodzi o główną obsługę błędów, Go w wersji 1.13 wprowadziło "owijanie błędów" (error wrapping), co ułatwia zrozumienie "łańcucha błędów" dzięki funkcjom takim jak `errors.Is()` oraz `errors.As()`.

## Zobacz także:

Wszystko na temat obsługi błędów w Go:

- Blog Go na temat obsługi błędów: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Efektywne Go – sekcja o obsłudze błędów: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Dokumentacja "owijania błędów" Go 1.13: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Wpis Dave’a Cheneya o strategiach obsługi błędów: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
