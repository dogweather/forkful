---
title:                "Go: Konwertowanie daty na ciąg znaków"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym elementem wielu aplikacji, szczególnie tych związanych z przetwarzaniem danych lub interfejsem użytkownika. W tym artykule dowiesz się, jak w łatwy sposób dokonać tej konwersji w języku Go.

## Jak to zrobić

Konwersja daty na string w języku Go jest bardzo prosta i wymaga zastosowania funkcji `Format()` z pakietu `time`. Przyjmijmy, że chcemy wyświetlić obecną datę w formacie "dd/mm/yyyy". W tym celu użyjemy następującego kodu:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	dateString := now.Format("02/01/2006")
	fmt.Println(dateString)
}
```

W wyniku otrzymamy następujący output:

`06/08/2019`

Możemy dostosować formatowanie daty, zmieniając kolejność cyfr i używając różnych znaków separujących (np. "/","-","."). Ważne jest jednak, aby zawsze używać określonych symboli podanych w dokumentacji pakietu `time`, ponieważ inaczej otrzymamy błąd.

## Głębsza analiza

W języku Go istnieje wiele różnych sposobów na konwersję daty na string, jednak najczęściej wykorzystuje się funkcję `Format()` z pakietu `time`. Dzięki temu rozwiązaniu możemy dostosować formatowanie daty do naszych potrzeb, bez konieczności używania dodatkowych bibliotek lub skomplikowanych operacji.

Inną popularną metodą na przekształcanie daty na string jest użycie funkcji `Parse()` z pakietu `time`. Pozwala ona na wczytanie daty w podanym formacie i przekonwertowanie jej na obiekt typu `time.Time`. Następnie możemy użyć funkcji `Format()` do wyświetlenia wczytanej daty w innym formacie.

## Zobacz także

- [Dokumentacja pakietu `time` w języku Go](https://golang.org/pkg/time/)
- [Przetwarzanie dat w języku Go - Poradnik Programera](https://blog.golang.org/go-time)
- [Konwersja daty na string w języku Go - Poradnik Wędkarski](https://marcusguarded.com/blog/konwersja-daty-w-go/)