---
title:                "Swift: Znajdowanie długości ciągu znaków"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak w języku Swift znaleźć długość ciągu znaków? Może masz do czynienia z tekstem, który chcesz podzielić na mniejsze fragmenty lub po prostu sprawdzić, czy długość kodu jest większa niż określona wartość. W tym artykule pokażemy Ci, jak to zrobić.

## Jak to zrobić

```Swift
let name = "Kasia"
let length = name.count
print("Długość imienia to: \(length)") // Output: Długość imienia to: 5
```

W powyższym przykładzie użyliśmy metody `count`, która jest dostępna dla typu `String` w języku Swift. Metoda ta zwraca nam liczbę znaków w danym ciągu. Możemy również użyć tej metody, aby sprawdzić długość dowolnego ciągu znaków, nie tylko imienia.

```Swift
let sentence = "To jest bardzo długi ciąg znaków!"
let length = sentence.count
print("Długość zdania to: \(length)") // Output: Długość zdania to: 31
```

Możemy również użyć metody `count` w połączeniu z pętlą `for-in`, aby przejrzeć każdy znak w ciągu znaków i zliczyć je.

```Swift
let word = "programowanie"
var counter = 0
for _ in word {
    counter += 1
}
print("Długość słowa to: \(counter)") // Output: Długość słowa to: 13
```

## Pogłębione zagadnienia

W języku Swift, długość ciągu znaków jest liczona w oparciu o Unicode, co oznacza, że znaki są traktowane indywidualnie, niezależnie od ich wyglądu. Na przykład litera "ń" będzie liczyć się jako jeden znak, pomimo swojego wyglądu na dwa znaki.

Inną ważną rzeczą do zapamiętania jest fakt, że metoda `count` może zwrócić liczbę znaków, a nie liczbę bajtów. W niektórych przypadkach, szczególnie przy obsłudze znaków spoza ASCII, liczba bajtów i liczba znaków mogą różnić się w przypadku niektórych operacji. W takich przypadkach warto użyć metody `utf8.count`, która zwraca liczbę bajtów w danym ciągu.

## Zobacz także

- [Dokumentacja Apple na temat obiektów typu String w języku Swift](https://developer.apple.com/documentation/swift/string)
- [Tutorial na temat ciągów znaków w języku Swift](https://www.raywenderlich.com/152611/swift-4-strings-cheat-sheet)
- [Przewodnik po Unicode w języku Swift](https://www.hackingwithswift.com/articles/69/whats-new-in-swift-5-1)