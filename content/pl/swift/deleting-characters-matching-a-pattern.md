---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Usuwanie znaków pasujących do wzorca w Swift: Jak? Dlaczego? I inne szczegóły

## Co i dlaczego?

Usuwanie znaków pasujących do wzorca to metoda usunięcia pewnego zbioru znaków z ciągu znaków, bazując na zdefiniowanym wzorcu. Programiści robią to, aby manipulować danymi tekstowymi, na przykład, aby oczyścić dane wejściowe lub usunąć niepotrzebne informacje.

## Jak to zrobić:

Swift oferuje wiele metod, które pozwalają usunąć znaki dopasowane do wzorca. Oto przykład:

```Swift
var str = "Witaj, Swift!"
let disallowedChars = CharacterSet(charactersIn: ",!")
str = str.components(separatedBy: disallowedChars).joined()
print(str) // "Witaj Swift"
```
W tym kodzie, używamy metody 'components(separatedBy:)' żeby rozdzielić nasz ciąg 'str' na podciągi, które są potem połączone bez separatorów, co daje nam nasz oczyszczony ciąg.

## Deep Dive:

* **Kontekst historyczny**: Swift, od momentu swojego powstania w 2014 roku przez Apple, zadbał o dostarczenie programistom wydajnych i elastycznych narzędzi do pracy z ciągami znaków.

* **Alternatywy**: Swift oferuje również inne metody manipulacji ciągami znaków, takie jak zamiana znaków pasujących do wzorca na inne. Na przykład, można użyć metody 'replacingOccurrences(of:with:)':

```Swift
var str = "Witaj, Swift!"
str = str.replacingOccurrences(of: ",", with: "")
str = str.replacingOccurrences(of: "!", with: "")
print(str) // "Witaj Swift"
```

* **Szczegóły implementacji**: Gdy manipulujemy ciągami znaków w Swift, ważne jest, aby zwrócić uwagę na kwestie wydajności. Operacje na ciągach mogą być kosztowne pod względem zużycia pamięci i czasu procesora, szczególnie dla dużych ciągów danych.

## Zobacz też:
