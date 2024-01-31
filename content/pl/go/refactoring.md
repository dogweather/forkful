---
title:                "Refaktoryzacja"
date:                  2024-01-26T01:36:58.118807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/refactoring.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Refaktoryzacja to proces restrukturyzacji istniejącego kodu komputerowego bez zmiany jego zewnętrznego zachowania. Programiści robią to, aby poprawić atrybuty niefunkcjonalne oprogramowania, takie jak czytelność i łatwość utrzymania, co może sprawić, że kod będzie łatwiejszy do zrozumienia, zmniejszy złożoność oraz ułatwi znajdowanie błędów.

## Jak to zrobić:
Zanurzmy się w prostym przykładzie refaktoryzacji kodu w Go. Weźmiemy fragment kodu, który oblicza średnią z ciągu liczbowego i zrefaktoryzujemy go dla większej klarowności i ponownego użycia.

Oryginalny kod:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Average:", average)
}
```

Zrefaktoryzowany kod:
```Go
package main

import "fmt"

// CalculateAverage pobiera ciąg float64 i zwraca średnią.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Average:", average)
}
```

W zrefaktoryzowanym kodzie, logika obliczania średniej została wyodrębniona do osobnej funkcji o nazwie `CalculateAverage`. Sprawia to, że funkcja `main` jest bardziej zwięzła, a logika obliczania średniej jest ponownie używalna i testowalna.

## Pogłębiona analiza
Refaktoryzacja kodu nie jest nowoczesnym konceptem; precedensy jej użycia datują się na czas przed powszechnym wykorzystaniem komputerów. Prawdopodobnie praktyka ta zaczęła się w dziedzinie inżynierii mechanicznej lub nawet wcześniej. W oprogramowaniu stała się bardziej sformalizowana wraz z nadejściem programowania zorientowanego obiektowo i ekstremalnego programowania (XP) w latach 90-tych, szczególnie pod wpływem przełomowej książki Martina Fowlera "Refaktoryzacja: Ulepszanie projektu istniejącego kodu".

Istnieje wiele technik refaktoryzacji, od prostych, takich jak przemianowanie zmiennych dla lepszej klarowności, po bardziej skomplikowane wzorce, takie jak wyodrębnianie metod lub klas. Kluczem jest dokonywanie małych, inkrementalnych zmian, które nie modyfikują funkcjonalności oprogramowania, ale poprawiają wewnętrzną strukturę.

Przy użyciu Go, refaktoryzacja może być stosunkowo prosta dzięki prostocie języka i potężnej bibliotece standardowej. Jednakże nadal ważne jest posiadanie dobrego zestawu testów jednostkowych, aby upewnić się, że refaktoryzacja nie wprowadzi błędów. Narzędzia takie jak `gorename` i `gofmt` pomagają zautomatyzować niektóre z procesów, a środowiska IDE często mają wbudowane wsparcie dla refaktoryzacji.

Oprócz manualnej refaktoryzacji dostępne są również narzędzia do automatycznej refaktoryzacji kodu dla Go, takie jak narzędzia refaktoryzacyjne GoLand i Go Refactor. Chociaż mogą przyspieszyć proces, nie zastępują one zrozumienia kodu i dokonywania przemyślanych zmian.

## Zobacz także
 - [Refaktoryzacja w Go: Proste jest piękne](https://go.dev/blog/slices)
 - [Efektywne Go: Refaktoryzacja z interfejsami](https://go.dev/doc/effective_go#interfaces)
 - [Strona o refaktoryzacji Martina Fowlera](https://refactoring.com/)
 - [Narzędzia refaktoryzacyjne GoLand](https://www.jetbrains.com/go/features/refactorings/)
