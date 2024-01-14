---
title:    "Go: Pobieranie bieżącej daty"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele aplikacji i systemów informatycznych wymaga aktualnej daty do prawidłowego działania. Bez niej nie byłoby możliwe śledzenie czasu, wyświetlanie wydarzeń czy generowanie raportów. W tym wpisie dowiesz się jak w języku Go uzyskać bieżącą datę oraz jak ją wykorzystać w swoim kodzie.

## Jak to zrobić

Pobranie aktualnej daty w języku Go jest bardzo proste. Wystarczy użyć funkcji `Now()` z biblioteki `time` i przypisać ją do zmiennej. Poniżej znajduje się przykładowe użycie tej funkcji:

```Go
import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Now()
    fmt.Println(currentDate)
}
```

Wywołanie tej funkcji zwróci nam obecną datę wraz z godziną. Możemy także określić jak chcemy aby została wyświetlona poprzez użycie formatowania:

```Go
currentDate := time.Now().Format("02.01.2006 15:04")
```

W powyższym przykładzie użyłem przyjętego standardu formatowania dat w Go. Możemy oczywiście dostosować go do naszych potrzeb. Pełna lista opcji formatowania dostępna jest w dokumentacji języka Go.

## Głębszy zanurzenie

Dokładne zrozumienie sposobu działania funkcji `Now()` może być przydatne w przypadku bardziej skomplikowanych projektów. W języku Go, czas jest reprezentowany przez strukturę `Time`, która zawiera informacje o dniu, miesiącu, roku, godzinie, minucie, sekundzie, strefie czasowej oraz innych szczegółach. Funkcja `Now()` zwraca wartość tego typu, co pozwala na dokładne operowanie na czasie w naszym programie.

## Zobacz też

- Dokumentacja języka Go: https://golang.org/doc/
- Przykładowe projekty w Go: https://github.com/golang/example