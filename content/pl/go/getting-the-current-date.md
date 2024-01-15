---
title:                "Uzyskiwanie aktualnej daty"
html_title:           "Go: Uzyskiwanie aktualnej daty"
simple_title:         "Uzyskiwanie aktualnej daty"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Aktualna wersja języka Go oferuje wiele funkcjonalności, jednak jedną z najważniejszych i najczęściej wykorzystywanych jest możliwość pobierania aktualnej daty. Jest to kluczowa funkcja, która jest niezbędna w wielu programach, szczególnie tych, które wymagają śledzenia czasu lub daty wydarzenia.

## Jak To Zrobić

Aby pobrać aktualną datę w języku Go, możemy skorzystać z wbudowanej funkcji "Now()", która zwraca obiekt typu "Time". Przykładowy kod poniżej pokazuje, jak użyć tej funkcji:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Now()
    fmt.Println(currentDate)
}
```

Output:

```
2021-05-22 15:30:00 +0000 UTC
```

Powyższy kod najpierw importuje paczkę "fmt", która jest potrzebna do wyświetlania wyników w konsoli. Następnie importujemy również paczkę "time", która zawiera funkcję "Now()". W funkcji main pobieramy aktualną datę przy użyciu tej funkcji i przypisujemy ją do zmiennej "currentDate". W końcu wyświetlamy tę zmienną za pomocą funkcji "Println" z paczki "fmt". Jak widać, w wyniku otrzymujemy datę wraz z czasem i strefą czasową.

Możemy także sformatować wyjście, aby wyświetlić tylko wybraną część daty, jak na przykład rok, miesiąc lub dzień. W poniższym przykładzie wyświetlana jest tylko nazwa miesiąca:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Now()
    fmt.Println(currentDate.Month())
}
```

Output:

```
May
```

Możemy także wykorzystać metodę "Format()" na obiekcie "Time", aby dostosować wyjście daty. Przykładowy kod poniżej wyświetla datę w formacie "dd/mm/yyyy":

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Now()
    fmt.Println(currentDate.Format("02/01/2006"))
}
```

Output:

```
22/05/2021
```

## W Jaki Sposób

Funkcja "Now()" wykorzystuje strefę czasową oraz ustawienia systemu operacyjnego do pobrania aktualnej daty. Dzięki temu nie musimy martwić się o różnice w czasie na różnych komputerach.

Ponadto, język Go oferuje również wiele innych metod, które umożliwiają operacje na datach i czasie, takich jak porównywanie, dodawanie i odejmowanie czasu czy też pobieranie konkretnej części daty.

## Zanurzenie Się

Jeśli chcemy poznać więcej szczegółów na temat pobierania daty w języku Go, warto zapoznać się z dokumentacją funkcji "Now()" oraz innych metod z paczki "time". Możemy także wykorzystać różnego rodzaju biblioteki lub pakiety, które rozszerzają możliwości pracy z datami i czasem.

## Zobacz Również

- [Dokumentacja języka Go](https://golang.org/ref/spec)
- [Paczka "time" w języku Go](https://golang.org/pkg/time/)
- [Przydatne paczki i biblioteki do manipulacji datami w języku Go](https://github.com/augmentable-dev/awesome-ai-curriculum/blob/master/dates-and-time-libraries-and-tools.md#go)