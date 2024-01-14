---
title:                "Go: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przeszłości lub przyszłości może być niezbędne w różnych przypadkach, na przykład przy tworzeniu aplikacji kalendarza lub bezpiecznego czasu wygaśnięcia ważności pewnych informacji. Niezależnie od powodu, znajomość sposobu przeliczania daty jest ważną umiejętnością, jeśli pracujesz w środowisku Go.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w języku Go, musimy skorzystać z funkcji `AddDate()` z pakietu `time`, która pozwala nam dodać lub odjąć określoną liczbę lat, miesięcy i dni od podanej daty.

Najpierw musimy zadeklarować zmienną `t` typu `time.Time`, która będzie naszą podstawową datą do obliczeń. Następnie, używając funkcji `AddDate()`, możemy dodać lub odjąć żądany czas, w tym przypadku, 1 rok, 2 miesiące i 3 dni, jak w poniższym przykładzie:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println(t) // bieżąca data

	t = t.AddDate(1, 2, 3) // dodaj 1 rok, 2 miesiące i 3 dni
	fmt.Println(t) // obliczona data
}
```

Output:

```
2021-10-20 21:45:00.2768895 +0200 CEST m=+0.003000001
2023-12-23 21:45:00.2768895 +0100 CET m=+0.003000001
```

Pamiętaj, aby w przypadku odliczania czasu w przeszłości używać ujemnych wartości dla lat, miesięcy i dni.

## Deep Dive

W języku Go istnieją również inne funkcje oraz metody z pakietu `time`, które pozwalają nam na bardziej elastyczne obliczanie daty. Na przykład, możemy użyć metody `Date()` do utworzenia daty na podstawie podanych parametrów, takich jak rok, miesiąc i dzień.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	d := time.Date(2021, 10, 20, 0, 0, 0, 0, time.UTC) // utwórz datę 20 października 2021
	fmt.Println(d)

	d = d.AddDate(0, 0, -30) // odlicz 30 dni w przeszłości
	fmt.Println(d)
}
```

Output:

```
2021-10-20 00:00:00 +0000 UTC
2021-09-20 00:00:00 +0000 UTC
```

W pakiecie `time` znajduje się również wiele innych przydatnych funkcji związanych z datami oraz obsługą strefy czasowej, więc zachęcamy do zapoznania się z nimi, aby rozszerzyć swoją wiedzę na ten temat.

## Zobacz również

- Dokumentacja pakietu `time` w języku Go: https://pkg.go.dev/time
- Poradnik o manipulacji datami i czasami w języku Go: https://www.ardanlabs.com/blog/2013/12/manipulating-dates-and-times-in-go.html
- Przydatna biblioteka do obliczania dat: https://github.com/jinzhu/now