---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego

Generowanie liczb losowych to sposób tworzenia ciągów numerów, które nie mają zaplanowanego wzoru. Programiści robią to, aby dodać element niespodzianki do swoich aplikacji, imitować złożoność rzeczywistości lub testować złożone sytuacje.

## Jak to zrobić

Tworzenie losowych liczb w Go jest dość prostym procesem:

```go
package main

import (
   "fmt"
   "math/rand"
   "time"
)

func main() {
   rand.Seed(time.Now().UnixNano())
   num := rand.Intn(100) // Generuje liczbę losową między 0 a 99
   fmt.Println(num)
}
```

Po uruchomieniu powyższego kodu, zobaczysz na wyjściu losową liczbę między 0 a 99.

## Głębsze zrozumienie

Generowanie liczb losowych ma długą historię, nawet w kontekście informatyki. Istnieją różne sposoby generowania liczb losowych, takie jak użycie ziarna czasu, jak pokazano powyżej, lub użycie bardziej skomplikowanych algorytmów, które mają na celu zwiększenie "randomowości" generowanych liczb.

Alternatywnymi metodami generowania liczb losowych w Go jest użycie pakietu `crypto/rand`, który jest bardziej bezpieczny, ale także wolniejszy. Również pakiet `math/rand` zapewnia różne typy generatorów liczb losowych, które można wykorzystać do różnych zastosowań.

Co do szczegółów implementacji, Go korzysta z generatora liczb pseudolosowych bazującego na algorytmie PCG. To modernizuje tradycyjne metody generowania liczb losowych, zapewniając lepszą jakość i szybkość.

## Zobacz też

1. [Dokumentacja Go na temat pakietu math/rand](https://golang.org/pkg/math/rand/)
2. [Dokumentacja Go na temat pakietu crypto/rand](https://golang.org/pkg/crypto/rand/)
4. [Opis algorytmu PCG](https://www.pcg-random.org/)