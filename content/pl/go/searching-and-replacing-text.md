---
title:                "Go: Szukanie i zamiana tekstu"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego
Witajcie! Jeśli jesteś programistą, który często pracuje z tekstem, na pewno zdarza się Tobie przeszukiwać i zamieniać fragmenty tekstu w swoim kodzie. W tym poście przyjrzymy się temu, dlaczego jest to ważne i jak to zrobić w języku Go.

## Jak to zrobić
Aby przeszukiwać i zamieniać tekst w Go, musimy skorzystać z funkcji `strings.Replace()`. Poniżej znajdziesz prosty przykład kodu, który zamienia słowo "Hello" na "Hola":

```Go
package main

import(
    "fmt"
    "strings"
)

func main() {
    text := "Hello world!"
    newText := strings.Replace(text, "Hello", "Hola", 1)
    fmt.Println(newText) // Prints "Hola world!"
}
```

Funkcja ta przyjmuje cztery argumenty: napis, który chcemy zamienić, fragment, który chcemy zastąpić, wartość zastępującą oraz liczbę wystąpień, które chcemy zamienić. W powyższym przykładzie użyliśmy wartości 1, więc zastąpiliśmy tylko pierwsze wystąpienie słowa "Hello". Jeśli chcesz zastąpić wszystkie wystąpienia, po prostu użyj parametru -1.

## Deep Dive
Jeśli chcesz dowiedzieć się więcej na temat przeszukiwania i zamieniania tekstu w Go, warto zapoznać się z dokumentacją języka. Istnieje wiele różnych metod i funkcji, które mogą pomóc Ci w manipulowaniu tekstem, a znajomość ich może znacznie usprawnić Twoją pracę.

## Zobacz też
Jeśli chcesz zgłębić temat jeszcze bardziej, polecamy zapoznać się z tymi zasobami:

- Dokumentacja Go - https://golang.org/doc
- Oficjalne forum języka Go - https://forum.golangbridge.org/
- Poradniki i artykuły na temat Go - https://blog.golang.org/
- Społeczność Go na Reddit - https://www.reddit.com/r/golang/

Dzięki funkcji `strings.Replace()` możemy łatwo przeszukiwać i zamieniać tekst w naszym kodzie. Mamy nadzieję, że ten artykuł okazał się dla Ciebie pomocny. Dziękujemy za przeczytanie!