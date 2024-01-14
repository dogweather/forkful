---
title:                "Go: Wydrukuj wyniki debugowania"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często musimy pracować z bardzo złożonym kodem, a czasami po prostu chcemy sprawdzić, czy nasza aplikacja działa poprawnie. W takich sytuacjach pomocne jest wypisywanie debug outputu, czyli informacji o tym, co dzieje się wewnątrz naszego programu. Może to być niezwykle przydatne w celu zlokalizowania błędów lub zrozumienia przepływu danych w naszej aplikacji.

## Jak to zrobić

W języku Go mamy do dyspozycji wiele różnych sposobów drukowania debug outputu. Oto kilka przykładów z wykorzystaniem funkcji fmt.Println():

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
    fmt.Println(2 + 2)
}
```

W powyższym przykładzie używamy fmt.Println() do wypisania tekstu "Hello, World!" oraz wyniku działania 2 + 2, czyli liczby 4.

Możemy również wykorzystać funkcję fmt.Printf() do formatowania wyjścia w bardziej kontrolowany sposób:

```Go
package main

import "fmt"

func main() {
    fmt.Printf("Liczba: %d\n", 42)
}
```

W powyższym przykładzie używamy %d do wypisania liczby całkowitej, a następnie podajemy wartość, która ma zostać wstawiona w to miejsce.

Innym sposobem na drukowanie debug outputu jest wykorzystanie paczki log, która oferuje więcej opcji, takich jak wypisywanie informacji o błędach oraz podawanie linii i pliku, w którym zostało wywołane to drukowanie:

```Go
package main

import "log"

func main() {
    log.Println("Error occurred")
}
```

## Deep Dive

W celu bardziej zaawansowanego drukowania debug outputu w języku Go możemy skorzystać z paczki "runtime/debug", która umożliwia uzyskiwanie szczegółowych informacji o wykonywanym programie, takich jak stos wywołań, informacje o gorutynach czy alokacje pamięci.

Jedną z najważniejszych funkcji tej paczki jest PrintStack(), która pozwala wypisać aktualny stos wywołania programu. Używając jej w razie wystąpienia błędu, możemy łatwiej zlokalizować jego przyczynę.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o drukowaniu debug outputu w języku Go, polecamy zapoznać się z poniższymi źródłami:

- [Oficjalna dokumentacja fmt](https://golang.org/pkg/fmt/)
- [Oficjalna dokumentacja log](https://golang.org/pkg/log/)
- [Oficjalna dokumentacja paczki runtime/debug](https://golang.org/pkg/runtime/debug/)
- [Artykuł "Debugging techniques in Go"](https://sosedoff.com/2015/05/08/debugging-techniques-in-go.html)
- [Film "Debugging and profiling with Go"](https://www.youtube.com/watch?v=PAAkCSZUG1c)