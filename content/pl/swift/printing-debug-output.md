---
title:    "Swift: Wydrukowanie wyjścia debugowania"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Programiści często znajdują się w sytuacjach, gdzie muszą znajdować i naprawiać błędy w swoim kodzie. Jednym z najważniejszych narzędzi w takich sytuacjach jest wypisywanie danych debugujących. Pozwala to programiście na lepsze zrozumienie, co dzieje się w jego programie i szybsze znalezienie i naprawienie błędu.

## Jak To Zrobić

W Swift, istnieje kilka metod, aby wyświetlić dane debugujące w konsoli.

#### Podstawowe użycie funkcji print()
```Swift
print("Hello, world!")
```

#### Wyświetlanie zmiennej
```Swift
let number = 5
print("Liczba wynosi: \(number)")
```

#### Debugowanie warunków
```Swift
assert(number > 10, "Liczba musi być większa niż 10.")
```

#### Wyświetlanie danych kolekcji
```Swift
let fruits = ["jabłko", "banan", "pomarańcza"]
print("Owoce: \(fruits)")
```

#### Używanie funkcji debugPrint()
```Swift
debugPrint("Debugowanie") // wyświetli "Debugowanie"
```

## Głębsza Analiza

Funkcje print() i debugPrint() znajdują się w standardowej bibliotece języka Swift i są używane do wypisywania tekstu i wartości zmiennych. Istnieje także możliwość ustawienia poziomu debugowania w projekcie, co pozwala na wyświetlenie informacji debugujących tylko w trybie developerskim, a nie w wersji produkcyjnej aplikacji.

Inną przydatną funkcją jest breakpoint, który pozwala na zatrzymanie wykonywania kodu w określonym miejscu, co ułatwia debugowanie.

## Zobacz Również

- [Debugging in Swift](https://docs.swift.org/swift-book/LanguageGuide/Debugging.html)
- [Using the Swift Debugger](https://docs.swift.org/swift-book/ReferenceManual/LLDBDebugging.html)
- [How to Debug your Code in Swift](https://blog.usejournal.com/how-to-debug-your-code-in-swift-9cc8ccf847b)