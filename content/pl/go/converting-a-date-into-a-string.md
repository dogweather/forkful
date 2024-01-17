---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Go: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwertowanie daty na ciąg znaków jest jednym z najczęściej wykonywanych zadań przez programistów. Polega to na przekształceniu daty w formacie liczbowym na czytelny dla człowieka ciąg znaków, który można wykorzystać do wyświetlenia lub przechowywania daty. Jest to niezbędne w wielu aplikacjach, na przykład w bazach danych czy generowaniu raportów.

## Jak to zrobić:

Go oferuje wbudowany sposób na konwertowanie daty do ciągu znaków za pomocą funkcji "Format" z pakietu "time". W poniższym przykładzie pokazane jest jak wyświetlić aktualną datę w formacie "DD-MM-RRRR":

    ```Go
    import (
      "fmt"
      "time"
    )

    func main() {
      now := time.Now()
      formattedDate := now.Format("02-01-2006")
      fmt.Println(formattedDate) //output: 05-12-2021
    }
    ```

Można również użyć specjalnych znaków, aby określić inne formaty daty, na przykład "Mon Jan _2 15:04:05 2006 MST":

    ```Go
    func main() {
      now := time.Now()
      formattedDate := now.Format("Mon Jan _2 15:04:05 2006 MST")
      fmt.Println(formattedDate) //output: Sun Dec 5 11:22:36 2021 MST
    }
    ```

Zwróć uwagę, że w formacie należy używać liczby 2 dla dnia miesiąca, 1 dla miesiąca i 2006 dla roku, ponieważ jest to konwencja w języku Go.

## Głębsza analiza:

Konwersja daty na ciąg znaków jest niezbędnym wyzwaniem, ponieważ różne kraje stosują różne formaty dat. Dlatego ważne jest, aby dokładnie zdefiniować format w celu uniknięcia błędów i niejednoznaczności. W języku Go jest dostępnych wiele funkcji i pakietów do pracy z datami, więc warto zapoznać się z dokumentacją, aby wybrać najlepszą opcję dla danego projektu.

## Zobacz także:

- Dokumentacja pakietu "time" w języku Go: https://golang.org/pkg/time/
- Porównanie różnych metod konwersji dat w języku Go: https://blog.golang.org/go-data-formats