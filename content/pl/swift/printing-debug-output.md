---
title:                "Swift: Wyświetlanie wyników debugowania"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, podczas pisania kodu Swift, może być trudno zrozumieć, dlaczego program nie działa tak, jak powinien. W takiej sytuacji bardzo przydatne jest wyświetlanie komunikatów debugowania w celu zlokalizowania błędów i poprawienia działania aplikacji.

## Jak To Zrobić

Istnieje kilka metod wyświetlania komunikatów debugowania w języku Swift. Jedną z nich jest użycie funkcji print(), która pozwala wypisać na konsoli dowolny obiekt lub wartość, np.:

```Swift
print("Witaj, jestem programem Swift.")
// Wynik: Witaj, jestem programem Swift.
```

Można także wykorzystać funkcję debugPrint(), która pozwala na bardziej zaawansowane formatowanie stringów, np.:

```Swift
let imie = "Karolina"
debugPrint("Cześć, jestem \(imie).")
// Wynik: "Cześć, jestem \"Karolina\"."
```

W celu wyświetlenia komunikatu debugowania dla konkretnej zmiennej lub stałej, można również skorzystać z polecenia print(), np.:

```Swift
let liczba = 10
print("Moja liczba to \(liczba)")
// Wynik: Moja liczba to 10.
```

Pamiętaj, że funkcje print() i debugPrint() działają tylko w trybie debug, dlatego przed przesłaniem aplikacji do App Store warto usunąć te wywołania lub dodać odpowiednie warunki, np.:

```Swift
#if DEBUG
    print("Debugowanie: \(jakasZmienna)")
#endif
```

Istnieje również możliwość wyświetlania komunikatów debugowania w konsoli Xcode przy użyciu funkcji `NSLog()`, jednak ta metoda jest przestarzała i proponowane jest korzystanie z funkcji print() i debugPrint().

## Deep Dive

Wyświetlanie komunikatów debugowania jest szczególnie przydatne przy debugowaniu aplikacji w czasie rzeczywistym, kiedy nie można skorzystać z debuggera. Dzięki nim można śledzić wartości zmiennych i stałych w poszczególnych momentach działania aplikacji, co ułatwia zlokalizowanie błędów.

Jednak należy pamiętać, że zbyt wiele komunikatów debugowania może spowolnić działanie aplikacji i utrudnić jej testowanie. Dlatego należy wyświetlać tylko istotne informacje i usuwać je przed udostępnieniem aplikacji.

## Zobacz Również

- [Debugowanie aplikacji w Xcode](https://developer.apple.com/xcode/debugging/)
- [Podstawy języka Swift](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/)
- [Użycie funkcji print() i debugPrint() w języku Swift](https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID103)