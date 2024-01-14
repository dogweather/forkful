---
title:    "Swift: Pisanie testów"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy?

Pisanie testów jest niezwykle ważnym aspektem programowania w Swift. Testy pozwalają nam na sprawdzenie naszego kodu i upewnienie się, że działa on zgodnie z oczekiwaniami. Pozwala to również na szybsze wykrycie i naprawienie ewentualnych błędów w kodzie. W tym artykule przyjrzymy się, dlaczego warto pisać testy i jak to zrobić w języku Swift.

## Jak pisać testy?

Aby napisać testy w Swift, możemy skorzystać z wbudowanego frameworku XCTest. Zaczynamy od importowania go do naszego pliku kodu za pomocą słowa kluczowego `import XCTest`.

Następnie, tworzymy naszą pierwszą funkcję testową, używając klasy `XCTestCase`. 

```Swift
class MyTests: XCTestCase {
  func testSum() {
    let a = 5
    let b = 7
    let result = a + b
    XCTAssertEqual(result, 12)
  }
}
```

W powyższym przykładzie, funkcja `testSum()` sprawdza, czy wynik dodawania dwóch liczb jest równy oczekiwanemu wynikowi. Używając metody `XCTestCase` `XCTAssertEqual()`, możemy łatwo porównać oczekiwany wynik z rzeczywistym.

Po napisaniu funkcji testowej, musimy jeszcze umieścić ją w jednostce testującej (ang. test suite) poprzez użycie dekoratora `@testable import` oraz dodanie jej do funkcji `static func allTests() -> [(String, (ClassName) -> (...) throws -> Void)]`.

```Swift
@testable import MyProject
  static var allTests = [
    ("testSum", testSum),
  ]
```

Teraz, możemy uruchomić nasze testy, korzystając z opcji `Product → Test` w menu Xcode. Jeżeli wszystkie testy przechodzą pomyślnie, zobaczymy zielony znak ✓ obok nazwy każdej funkcji testowej. W przeciwnym wypadku, zobaczymy czerwony znak ✗ oraz informacje o nieprawidłowym wyniku lub błędzie.

## Deep Dive

Pisanie testów wymaga od nas myślenia o naszym kodzie w sposób systematyczny i dokładny. Musimy przewidzieć wszystkie możliwe scenariusze i upewnić się, że nasz kod je obsługuje. Warto również pamiętać o tym, aby nasze testy były niezależne od siebie i od innych. Oznacza to, że wynik jednej funkcji testowej nie powinien być zależny od wyniku innej funkcji testowej.

Istnieje wiele innych metod i technik pisania testów w języku Swift, takich jak testy jednostkowe, testy integracyjne czy testy UI. W dalszym rozwoju swojej kariery programisty Swift, warto zgłębiać te zagadnienia i pogłębiać swoją wiedzę na temat pisania testów.

## Zobacz także

- [Podręcznik Swift: Testowanie i debugowanie kodu](https://docs.swift.org/swift-book/LanguageGuide/Testing.html)
- [Dokumentacja Xcode: Pisanie testów w języku Swift](https://developer.apple.com/documentation/xcode/writing_tests_for_your_code)
- [Artykuł: Pisanie testów w języku Swift](https://medium.com/@ssribhavyaswamy/tutorials-on-writing-tests-in-swift-4ef029bfd8b0)