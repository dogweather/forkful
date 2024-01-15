---
title:                "Zamiana stringa na Wielkie Litery"
html_title:           "Swift: Zamiana stringa na Wielkie Litery"
simple_title:         "Zamiana stringa na Wielkie Litery"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami będziesz musiał zmienić wielkość liter w ciągu znaków w swoim kodzie Swift. Może być to wymagane, na przykład, gdy chcesz wyświetlić imię i nazwisko w formacie z wielkimi literami. W takich sytuacjach używamy funkcji do zmiany wielkości liter w ciągu znaków.

## Jak to zrobić

Zmiana wielkości liter w ciągu zanaków w Swift jest bardzo prosta i możesz to zrobić na kilka różnych sposobów. Najprostszą metodą jest użycie wbudowanej funkcji `uppercased()`:

```Swift
let name = "jan kowalski"

let upperCaseName = name.uppercased()

print(upperCaseName) // Wypisze "JAN KOWALSKI"
```

Możesz również użyć funkcji `lowercased()` jeśli chcesz zmienić wszystkie litery na małe:

```Swift
let name = "JAN KOWALSKI"

let lowerCaseName = name.lowercased()

print(lowerCaseName) // Wypisze "jan kowalski"
```

Jeśli chcesz tylko zmienić pierwszą literę w ciągu na wielką, możesz użyć metody `capitalized()`:

```Swift
let name = "jan kowalski"

let capitalizedName = name.capitalized()

print(upperCaseName) // Wypisze "Jan Kowalski"
```

Wszystkie te funkcje mogą być również używane na dowolnym typie dziedziczącym po `String`, na przykład na zmiennej typu `NSString`:

```Swift
let name = NSString(string: "jan kowalski")

let upperCaseName = name.uppercased()

print(upperCaseName) // Wypisze "JAN KOWALSKI"
```

## Najważniejsze informacje

Warto również wiedzieć, że funkcje `uppercased()`, `lowercased()` i `capitalized()` zwracają nowy ciąg znaków, a nie zmieniają oryginalnego. Możesz więc zapisywać te wartości do nowych zmiennych lub po prostu wykorzystywać je bezpośrednio w swoim kodzie.

## Zobacz także

- Dokumentacja Apple: [Working with Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Tutorial na YouTube: [String Manipulation in Swift](https://www.youtube.com/watch?v=XZYJv8TqKMg)
- Przewodnik dla początkujących: [String Operations in Swift](https://www.geeksforgeeks.org/string-operations-in-swift/)