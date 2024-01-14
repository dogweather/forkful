---
title:    "Go: Znajdowanie długości łańcucha znaków"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości ciągu znaków może być niezbędne w wielu różnych aplikacjach w języku programowania Go. Na przykład, możemy chcieć sprawdzić długość hasła wprowadzonego przez użytkownika, aby upewnić się, że jest wystarczająco długie i bezpieczne. W tym artykule dowiesz się, jak obliczyć długość ciągu znaków w języku Go.

## Jak to zrobić

Obliczenie długości ciągu znaków w Go jest bardzo proste. Możemy użyć funkcji `len()` do pobrania długości ciągu. Następnie możemy wypisać lub użyć długości w inny sposób w naszym kodzie. Oto przykładowy kod:

```Go
package main

import "fmt"

func main() {
  name := "Poland"
  fmt.Println("Długość ciągu znaków dla słowa", name, "to", len(name))
}
```

Powyższy kod najpierw deklaruje zmienną `name`, która zawiera ciąg znaków "Poland". Następnie wyświetla komunikat, który oblicza długość ciągu znaków dla słowa "Poland". Wynik wynosi 6, ponieważ słowo "Poland" składa się z sześciu znaków.

Możemy również użyć funkcji `len()` w pętli for w celu ustalenia liczby iteracji. Oto przykład:

```Go
package main

import "fmt"

func main() {
  sentence := "To jest zdanie"
  for i := 0; i < len(sentence); i++ {
    fmt.Println("Iteracja", i+1, "wyświetla:", sentence[i])
  }
}
```

W powyższym przykładzie, pętla for będzie wykonywać się przez tyle iteracji, ile wynosi długość ciągu znaków w zmiennej `sentence`. W każdej iteracji, zostanie wyświetlony kolejny znak ze zmiennej `sentence`. Spróbuj zmienić wartość w zmiennej `sentence` i zobacz, jak będzie to wpływać na liczbę iteracji i wyświetlane znaki.

## Głębsza analiza

Funkcja `len()` w rzeczywistości zwraca liczbę bajtów w danym ciągu znaków, a nie liczbę samej długości. W przypadku ciągów znaków ASCII, liczba bajtów będzie odpowiadała liczbie znaków, ale w przypadku innych systemów znaków, ta liczba może się różnić. Dlatego oznacza to, że w przypadku niektórych aplikacji, szczególnie tych związanych z bezpieczeństwem, musimy dokładnie zrozumieć, co oznacza liczenie bajtów zamiast samej długości.

## Zobacz również

- Dokumentacja Go o funkcji `len()`: https://golang.org/ref/spec#Length_and_capacity
- Przykłady zastosowań funkcji `len()`: https://www.golangprograms.com/go-program-to-find-length-of-string.html
- Przydatne informacje o systemach znaków: https://godoc.org/golang.org/x/text/encoding