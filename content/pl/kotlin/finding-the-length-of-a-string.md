---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Kotlin: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?
Znalezienie długości ciągu znaków jest jedną z podstawowych operacji, które programiści muszą wykonywać podczas pisania kodu. Polega ona na określeniu ilości znaków w danym ciągu, czyli na policzeniu jego długości. W większości języków programowania istnieje wbudowana funkcja do wykonywania tej czynności.

## Jak to zrobić:
W języku Kotlin mamy do wyboru dwa sposoby na znalezienie długości ciągu:
```Kotlin
val str = "Hello World"
println(str.length) // Output: 11
// lub
println("Hello World".length) // Output: 11
```
W pierwszym przykładzie tworzymy zmienną zawierającą nasz ciąg znaków, a następnie wywołujemy na niej funkcję `length`. W drugim przykładzie korzystamy z wbudowanej funkcji `length` bezpośrednio na ciągu znaków.

## Wgląd w zagadnienie:
Funkcja `length` jest dostępna w większości języków programowania, nie tylko w Kotlinie. Jej implementacja może się różnić w zależności od języka, ale zazwyczaj polega na iterowaniu przez poszczególne znaki w ciągu i zliczaniu ich liczby. Alternatywnym sposobem na znalezienie długości ciągu może być użycie wbudowanej funkcji `size`, którą również można znaleźć w Kotlinie.

## Zobacz też:
- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/reference/functions.html#length)
- [Porównanie funkcji `length` i `size`](https://kotlinlang.org/docs/reference/whatsnew10.html#string-length-size-etc)