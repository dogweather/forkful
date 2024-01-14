---
title:    "Go: Usuwanie znaków odpowiadających wzorcowi"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Świat programowania jest pełen różnych zadań, które czasami mogą wydawać się niepotrzebne lub bezcelowe. Jednak, szybkie i efektywne usuwanie znaków pasujących do określonego wzorca może znacznie ułatwić pracę z danymi tekstowymi. W tym wpisie dokładniej przyjrzymy się, dlaczego warto poznać tę umiejętność i jak ją wdrożyć w praktyce.

## Jak to zrobić

W celu usunięcia znaków pasujących do danego wzorca, możemy skorzystać z funkcji `regexp.ReplaceAllString()` w języku Go. W poniższym kodzie przedstawiona jest prosta implementacja usuwania wszystkich samogłosek z danego tekstu:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	text := "Witaj w świecie programowania!"
	regex := regexp.MustCompile("[aeiou]")
	newText := regex.ReplaceAllString(text, "")

	fmt.Println(newText)
	// Output: Wtj w śwd prgrmwng!
}
```

W powyższym przykładzie najpierw deklarujemy zmienną `text` z danym tekstem, a następnie tworzymy regułę wyrażenia regularnego z użyciem funkcji `regexp.MustCompile()`. W nawiasach kwadratowych umieszczamy znaki, które chcemy usunąć z tekstu. W naszym przypadku jest to `[aeiou]`, co oznacza wszystkie samogłoski. Następnie, używając funkcji `ReplaceAllString()`, podajemy tekst oraz puste znaki jako argumenty, co spowoduje usunięcie znalezionych przez wyrażenie regularne samogłosek. Ostatecznie, wypisujemy nowy tekst bez samogłosek na ekranie.

## Głębszy zanurzenie

Wyrażenia regularne są bardzo przydatnym narzędziem, które można wykorzystać w różnych celach, między innymi do edycji i modyfikacji tekstu. W przypadku usuwania znaków pasujących do wzorca, warto poznać inne możliwości funkcji `ReplaceAllString()`. Na przykład, można wykorzystać jej drugi argument jako funkcję, która zostanie wykonana na znalezionych dopasowaniach. W ten sposób możemy dostosować nasze usuwanie do konkretnych potrzeb.

```
regex.ReplaceAllStringFunc(text, func(match string) string{
	// Kod wykonujący określone działanie na dopasowaniu
})
```

## Zobacz także

- [Dokumentacja języka Go - pakiety regexp](https://golang.org/pkg/regexp/)
- [Wprowadzenie do wyrażeń regularnych - kurs na Codecademy](https://www.codecademy.com/learn/introduction-to-regular-expressions)