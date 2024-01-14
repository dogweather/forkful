---
title:    "Swift: Znajdowanie długości ciągu znaków"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Często w programowaniu musimy operować na tekstach i jedną z podstawowych operacji jest sprawdzenie długości danego ciągu znaków. W tym artykule dowiesz się, jak znaleźć długość ciągu znaków w języku Swift.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w Swift, należy użyć metody `count` na zmiennej typu `String`. Oto przykładowy kod:

```Swift
var text = "Witaj świecie"
print(text.count) // Output: 13
```

W powyższym przykładzie, najpierw tworzymy zmienną `text` i przypisujemy do niej ciąg znaków "Witaj świecie". Następnie, korzystając z metody `count`, otrzymujemy liczbę znaków w tej zmiennej.

Możemy również użyć tej metody na stałych typu `String` oraz na wynikach funkcji lub działań na ciągach znaków. Na przykład:

```Swift
let sentence = "Programowanie w Swift jest zabawne!"
print(sentence.count) // Output: 34

let name = "Anna"
let greeting = "Cześć " + name
print(greeting.count) // Output: 9
```

W powyższych przykładach możemy zauważyć, że metoda `count` jest bardzo przydatna, gdy chcemy sprawdzić długość różnych ciągów znaków.

## Deep Dive

Metoda `count` jest dostępna w języku Swift dzięki temu, że każdy ciąg znaków jest reprezentowany przez typ `String`. Ten typ ma wiele przydatnych metod, takich jak `count`, które ułatwiają nam manipulowanie tekstem.

Dodatkowo, metoda `count` jest bardzo wydajna i działa dla różnych języków i znaków. Dzięki temu jest to niezawodne narzędzie do znajdowania długości ciągów znaków.

# Zobacz również

- [Dokumentacja Swift na temat typu String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Inne metody dostępne dla typu String](https://www.hackingwithswift.com/read/0/16/strings-and-integers-frequency-tables)
- [Przykłady zastosowania funkcji `count`](https://www.dotnetperls.com/square-swift)