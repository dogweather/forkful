---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Go: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu dni, miesięcy i lat od konkretnej daty. Programiści robią to, na przykład, aby ustalić, kiedy wygasa określony termin albo aby sprawdzić jak długo coś trwało.

## Jak to zrobić:

W języku Go, możemy skorzystać z pakietu `time` aby obliczyć datę w przyszłości lub przeszłości. Wystarczy wywołać metodę `AddDate` na obiekcie `time.Time`, podając ilość lat, miesięcy i dni jako parametry.

Zobaczmy przykład:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println("Dzisiaj:", t.Format("2006-01-02"))

	future := t.AddDate(0, 0, 10)
	fmt.Println("Za 10 dni:", future.Format("2006-01-02"))

	past := t.AddDate(0, -1, 0)
	fmt.Println("Miesiąc temu:", past.Format("2006-01-02"))
}
```

Po uruchomieniu powyższego kodu zobaczysz wydrukowane dzisiejszą datę, datę za 10 dni oraz datę miesiąca temu.

## Głębsze spojrzenie

Częste obliczanie daty w przyszłości lub przeszłości jest powszechne w programowaniu, a różne języki programowania mają różne metody na tego typu operacje. W Go jest to relatywnie proste dzięki wbudowanemu typowi `time.Time`. Z perspektywy historycznej, można powiedzieć że Go zaimplementowało to w bardziej prostej i zwięzłej formie niż wiele innych języków.

W Go, wartości `time.Time` są niezmienne, co oznacza, że kiedy dodajesz lub odejmujesz czas do danego obiektu `time.Time`, zwracany jest nowy obiekt a nie modyfikowany istniejący.

Jeśli potrzebujesz alternatywy do metody `AddDate`, możesz zastosować metodę `Add`, podając jako parametr obiekt `time.Duration`.

## Zobacz także

- Dokumentacja pakietu time w Go: https://pkg.go.dev/time
- Poradnik do stosowania czasu i daty w Go: https://yourbasic.org/golang/time-date-difference-format-parse
- Wprowadzenie do date i time w Go na blogu Go: https://blog.golang.org/time