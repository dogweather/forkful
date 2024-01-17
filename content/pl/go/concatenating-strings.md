---
title:                "Łączenie ciągów znaków"
html_title:           "Go: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Łączenie stringów to prosta operacja, która pozwala programistom łączyć zbiory znaków w jedną linię tekstu. Jest to bardzo przydatne przy tworzeniu dynamicznych wiadomości, np. w przypadku tworzenia wiadomości e-mail lub generowania raportów. Programiści korzystają z tej funkcji, aby oszczędzić czas i zapewnić elastyczność w tworzeniu tekstu.

## Jak to zrobić:
Go oferuje prosty sposób, aby połączyć stringi dzięki funkcji `strings.Join()`. Można ją wywołać na obiekcie typu `strings` i przekazać jej slice z stringami, które chcemy połączyć, oraz separator, który ma zostać wstawiony pomiędzy każdym z nich. Przykładowe wywołanie tej metody mogłoby wyglądać tak:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	mySlice := []string{"witaj", "w", "świecie"}
	fmt.Println(strings.Join(mySlice, " "))
}
```

W tym przypadku wyjściem będzie łańcuch `witaj w świecie`.

## Głębsze zanurzenie:
Funkcja `Join` ma swoje korzenie w bibliotece C, a jej implementacja jest oparta na algorytmie QuickSort, co sprawia, że jest bardzo wydajna. Alternatywną metodą jest użycie operatora `+`, jednak jest to mniej efektywne, ponieważ tworzy kopię stringa za każdym razem, gdy operator jest używany. Kolejną użyteczną funkcją do łączenia stringów w Go jest metoda `Sprintf`, która umożliwia formatowanie tekstu z wykorzystaniem specyfikatorów formatu.

## Zobacz również:
https://golang.org/pkg/strings/#Join - dokumentacja funkcji `strings.Join()`
https://golang.org/doc/effective_go.html - oficjalny poradnik języka Go
https://gobyexample.com/string-formatting - przykłady formatowania stringów w Go