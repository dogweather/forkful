---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Swift: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Czym jest i dlaczego?

Obliczenie daty w przyszłości lub w przeszłości jest częstym zagadnieniem w programowaniu, szczególnie w aplikacjach takich jak kalendarze, przypomnienia czy systemy rezerwacyjne. Programiści mogą wykorzystać tę funkcjonalność, aby automatycznie wyświetlać daty z przeszłości lub przyszłości w zależności od potrzeb użytkownika.

# Jak to zrobić?

```Swift 
let currentDate = Date() 
let futureDate = Calendar.current.date(byAdding: .day, value: 3, to: currentDate)
print(futureDate)
```
W powyższym przykładzie używamy klasy Date, aby określić obecną datę. Następnie używamy klasy Calendar, aby dodać do niej 3 dni i uzyskać datę w przyszłości. Ostatecznie wyświetlamy ją przy użyciu metody print().

```Swift
let currentDate = Date() 
let pastDate = Calendar.current.date(byAdding: .month, value: -2, to: currentDate)
print(pastDate)
```
W drugim przykładzie używamy tej samej klasy i metody, ale tym razem odejmujemy 2 miesiące od obecnej daty, aby uzyskać datę w przeszłości.

# Głębszy wgląd

Korzystanie z klasy Date i Calendar jest popularnym sposobem na obliczanie dat w przyszłości lub przeszłości, ale istnieją również inne metody, takie jak wykorzystanie biblioteki NSDate czy tworzenie własnych funkcji do obliczania dat. Wprowadzenie do języka Swift funkcjonalności typu daty było jednym z celów projektu, co ułatwia pracę programistom w tym zakresie.

# Zobacz także

Jeśli chcesz dowiedzieć się więcej o korzystaniu z dat w programowaniu w Swift, polecamy:

- [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date) - oficjalna dokumentacja Apple na temat klasy Date w języku Swift.
- [Ray Wenderlich's NSDate and Swift tutorial](https://www.raywenderlich.com/54201/nsdate-swift) - szczegółowy przewodnik po wykorzystaniu klasy NSDate w języku Swift.
- [Swift Date and Time calculation library](https://github.com/malcommac/SwiftDate) - biblioteka, która ułatwia obliczanie dat w różnych strefach czasowych i formatowaniu ich wyświetlania.