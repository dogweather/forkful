---
title:                "Go: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Połączenie ciągów znaków jest niezbędną czynnością podczas pisania programów, szczególnie jeśli pracujesz z tekstem. Przydatne jest w tworzeniu pełnych wiadomości, formatowania dokumentów i wielu innych zastosowań. W tym artykule dowiesz się, jak połączyć ciągi znaków w języku Go.

## Jak to zrobić

Poniższy kod przedstawia proste przykłady tworzenia i łączenia ciągów znaków w języku Go:

```Go
package main

import "fmt"

func main() {
    // Tworzenie dwóch ciągów znaków
    message1 := "Hello "
    message2 := "world!"

    // Połączenie ciągów znaków za pomocą operatora "+"
    fullMessage := message1 + message2
    fmt.Println(fullMessage) // Wyjście: Hello world!

    // Połączenie ciągów znaków za pomocą funkcji "Join" z pakietu "strings"
    import "strings"
    result := strings.Join([]string{message1, message2}, " ")
    fmt.Println(result) // Wyjście: Hello world!
}
```

Możesz również łączyć więcej niż dwa ciągi za pomocą funkcji "Join". W takim przypadku, ciągi muszą być podane w tablicy, a separator znajdujący się po ostatnim ciągu zostanie użyty do połączenia całej tablicy.

```Go
package main

import "fmt"

func main() {
    // Tworzenie trzech ciągów znaków
    message1 := "Programming"
    message2 := "is"
    message3 := "fun!"

    // Połączenie trzech ciągów za pomocą funkcji "Join"
    import "strings"
    result := strings.Join([]string{message1, message2, message3}, " ")
    fmt.Println(result) // Wyjście: Programming is fun!
}
```

## Deep Dive

Istnieją różne sposoby łączenia ciągów znaków w języku Go. Jednak najważniejszym punktem jest zapewnienie, że wszystkie połączone ciągi są typu "string". W przeciwnym razie, nie będą one mogły być dodane za pomocą operatora "+" lub użyte w funkcji "Join". Warto również zauważyć, że operacja łączenia ciągów jest wykonywana w czasie rzeczywistym i może wpływać na wydajność Twojego programu w zależności od ilości i długości łączonych ciągów.

## Zobacz również

- Dokumentacja Go: https://golang.org/doc/
- Przewodnik po języku Go: https://tour.golang.org/list
- Wideo o połączeniach w języku Go: https://www.youtube.com/watch?v=eKnYdNRU3sQ