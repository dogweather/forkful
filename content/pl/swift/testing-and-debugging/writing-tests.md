---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:05.567804-07:00
description: "Jak to zrobi\u0107: Swift wspiera testowanie poprzez swoje \u015Brodowisko\
  \ XCTest, kt\xF3re jest zintegrowane z Xcode. Mo\u017Cna pisa\u0107 testy jednostkowe\
  \ w celu weryfikacji\u2026"
lastmod: '2024-03-13T22:44:35.758555-06:00'
model: gpt-4-0125-preview
summary: "Swift wspiera testowanie poprzez swoje \u015Brodowisko XCTest, kt\xF3re\
  \ jest zintegrowane z Xcode."
title: "Pisanie test\xF3w"
weight: 36
---

## Jak to zrobić:
Swift wspiera testowanie poprzez swoje środowisko XCTest, które jest zintegrowane z Xcode. Można pisać testy jednostkowe w celu weryfikacji indywidualnych części kodu, na przykład funkcji, która oblicza sumę dwóch liczb.

```swift
import XCTest
@testable import TwojaAplikacja

class TestyTwojejAplikacji: XCTestCase {

    func testSumy() {
        let wynik = Kalkulator().suma(a: 1, b: 2)
        XCTAssertEqual(wynik, 3, "Funkcja sumująca nie zwróciła oczekiwanej wartości.")
    }
}
```

Aby uruchomić ten test, zwykle naciska się Command-U w Xcode. Wyjście w nawigatorze testów Xcode powie Ci, czy test przeszedł pomyślnie, czy nie.

Na przykład, pomyślne wyjście testu:
```
Przypadek testowy '-[TestyTwojejAplikacji testSumy]' zakończony sukcesem (0.005 sekund).
```

W przypadku bardziej zaawansowanych scenariuszy testowych możesz korzystać z bibliotek firm trzecich takich jak Quick/Nimble, które oferują bardziej ekspresyjną składnię do pisania testów.

Z Quick/Nimble możesz napisać ten sam test w następujący sposób:

```swift
// Dodaj Quick i Nimble do menedżera pakietów Swift lub użyj CocoaPods/Carthage, aby je zainstalować
import Quick
import Nimble
@testable import TwojaAplikacja

class SpecyfikacjaKalkulatora: QuickSpec {
    override func spec() {
        describe("Kalkulator") {
            context("kiedy sumuje liczby") {
                it("powinien zwrócić poprawną sumę") {
                    let kalkulator = Kalkulator()
                    expect(kalkulator.suma(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Uruchomienie tego testu dałoby Ci podobne wyjście w konsoli testowej lub logu narzędzia CI/CD, wskazujące, czy test się powiódł czy nie, z bardziej czytelnym formatem opisującym testy i oczekiwania.
