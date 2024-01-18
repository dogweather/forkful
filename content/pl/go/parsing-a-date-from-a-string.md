---
title:                "Przetwarzanie daty z ciągu"
html_title:           "Go: Przetwarzanie daty z ciągu"
simple_title:         "Przetwarzanie daty z ciągu"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Parsowanie daty z ciągu znaków jest procesem wyodrębniania informacji o dacie z ciągu znaków. Programiści często wykonują tę operację, ponieważ chcą przetwarzać dane z datami i potrzebują punktu odniesienia dla tych informacji.

## Jak to zrobić?

Przykładowy kod w języku Go i jego wynik:

```
package main

import (
	"fmt"
	"time"
)

func main() {
	layout := "January 2, 2006"
	str := "February 5, 2019"
	t, _ := time.Parse(layout, str)
	fmt.Println(t)
}
```

Wynik:
```
2019-02-05 00:00:00 +0000 UTC
```

## Głębsze Podejście

Parsowanie dat z ciągów znaków nie jest nowym problemem, ponieważ już w latach 70. XX wieku programiści musieli radzić sobie z różnymi reprezentacjami dat w różnych systemach. W Go, parsowanie dat z ciągów znaków jest realizowane za pomocą standardowej funkcji ```time.Parse()```. Alternatywnym rozwiązaniem może być używanie biblioteki ```strftime```, dostępnej w większości języków programowania.

## Zobacz także

- [Dokumentacja Go dotycząca funkcji time.Parse()](https://golang.org/pkg/time/#Parse)
- [Dokumentacja Python dotycząca biblioteki strftime](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)