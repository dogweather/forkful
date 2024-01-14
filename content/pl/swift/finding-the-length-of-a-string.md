---
title:    "Swift: Znajdowanie długości ciągu znaków"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości łańcucha znaków jest jedną z najbardziej podstawowych operacji w programowaniu. Możemy wykorzystać tę wiedzę w różnych algorytmach i funkcjach, dlatego ważne jest, aby dobrze poznać metodę obliczania długości łańcucha w języku Swift. 

## Jak to zrobić

Obliczanie długości łańcucha jest bardzo proste w języku Swift. Możemy użyć metody `count` na łańcuchu i otrzymać liczbę wszystkich znaków w nim zawartych. 

```Swift
let imie = "Anna"
print(imie.count) // Output: 4
```

Jeśli chcesz sprawdzić długość łańcucha złożonego z innych typów danych, możesz zastosować konwersję na typ `String` i następnie skorzystać z metody `count`. 

```Swift
let liczba = 12345
let liczbaJakoString = String(liczba)
print(liczbaJakoString.count) // Output: 5
```

Warto także pamiętać, że metoda `count` zwraca liczbę wszystkich znaków w łańcuchu, niezależnie od ich kodowania.

## Głębszy wgląd

Za każdym razem, gdy wywołujemy metodę `count` na łańcuchu, Swift musi przeiterować przez wszystkie znaki w celu ustalenia długości. Oznacza to, że złożoność obliczeniowa tej operacji wynosi O(n), gdzie n jest długością łańcucha.

Warto także zauważyć, że znaki Unicode, takie jak emoji czy litery ze znakami diakrytycznymi, są traktowane jako pojedyncze znaki, więc metoda `count` zwróci długość tego łańcucha jako 1, pomimo faktycznej długości większej niż 1.

## Zobacz także

- Dokumentacja Apple na temat metody `count` w języku Swift: https://developer.apple.com/documentation/swift/string/229-creating-and-composing-strings
- Wprowadzenie do Unicode w Swift: https://www.hackingwithswift.com/example-code/strings/whats-the-difference-between-string-and-nsstring
- Przewodnik po złożoności obliczeniowej w języku Swift: https://calvinkamoski.com/blog/everything_you_need_to_know_about_big_o_notation_in_swift.html