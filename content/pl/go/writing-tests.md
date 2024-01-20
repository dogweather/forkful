---
title:                "Pisanie testów"
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Pisanie testów to proces tworzenia skryptów, które automatycznie sprawdzają, czy kod działa prawidłowo. Programiści robią to, aby zapewnić jakość kodu i unikać błędów.

## How to: (Jak to zrobić?)

Testy w Go piszemy przy pomocy pakietu `testing`. Oto przykład prostej funkcji i testów do niej:

```Go
package main

import (
    "testing"
    "fmt"
)

func Add(a, b int) int {
    return a + b
}

func TestAdd(t *testing.T) {
    result := Add(2, 3)
    if result != 5 {
        t.Errorf("Add(2, 3) = %d; want 5", result)
    }
}

func main() {
    fmt.Println("Suma: ", Add(2, 3))
}
```

Wynik uruchomienia `go test`:

```
PASS
ok  	path/to/your/package	0.002s
```

## Deep Dive (Dogłębna analiza)

Testy w Go sięgają korzeniami początków języka. Alternatywnymi metodami są Behavior-Driven Development (BDD) z narzędziami jak GoConvey czy testing frameworks jak Testify. Implementacja testów w Go jest prosta, testy są kompilowane do pliku wykonywalnego i uruchamiane jako osobny proces, zapewniając izolację i bezpieczeństwo.

## See Also (Zobacz również)

- Oficjalna dokumentacja Go: [Testy](https://golang.org/pkg/testing/)
- Artykuł o testach w Go: [Just for func: Writing testable Go code](https://youtu.be/hVFEV-ieeew)
- GoConvey, BDD framework dla Go: [GoConvey on GitHub](https://github.com/smartystreets/goconvey)
- Testify, narzędzie do asercji i mockowania: [Testify on GitHub](https://github.com/stretchr/testify)