---
title:                "Swift: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Testowanie jest niezwykle ważnym elementem procesu pisania kodu. Pomaga ono upewnić się, że nasz kod działa poprawnie i zapobiega błędom w przyszłości. Pisząc testy, możemy mieć większą pewność, że nasza aplikacja będzie odporna na wszelkie ewentualne problemy.

## Jak to zrobić

Pisanie testów w języku Swift jest stosunkowo proste i wygodne dzięki wbudowanemu w język frameworkowi - XCTest. Najpierw musimy utworzyć nowy projekt w Xcode, a następnie dodać do niego tzw. "test target", czyli specjalne miejsce, w którym będziemy umieszczać nasze testy. 

Poniżej przedstawiam prosty przykład testu, który sprawdza poprawność funkcji dodawania dwóch liczb:

```Swift
// Importujemy XCTest, aby mieć dostęp do jego funkcji testujących
import XCTest

// Tworzymy nasz testowy "class"
class TestExample: XCTestCase {

    // Funkcja testowa, która będzie automatycznie uruchamiana przez XCTest
    func testAddition() {
        // Definiujemy dwie liczby, które chcemy dodać
        let num1 = 5
        let num2 = 10
        // Wywołujemy funkcję dodawania i oczekujemy, że jej wynik będzie równy 15
        XCTAssertEqual(add(num1: num1, num2: num2), 15)
    }
    
    // Funkcja dodająca dwie liczby, którą chcemy przetestować
    func add(num1: Int, num2: Int) -> Int {
        return num1 + num2
    }
    
}
```

Po uruchomieniu testu, otrzymamy rezultat "Test Passed". W ten sposób sprawdzamy, czy nasza funkcja dodawania działa poprawnie. Warto zwrócić uwagę na nazwę funkcji testowej - musi ona zaczynać się od słowa "test", aby została automatycznie wywołana przez XCTest.

## Deep Dive

Istnieje wiele róznych typów testów, które możemy pisać w języku Swift, takich jak testy jednostkowe, testy wydajnościowe czy testy interfejsu użytkownika. Ważne jest, aby pamiętać, że pisanie testów nie tylko sprawia, że nasz kod jest bardziej bezpieczny i niezawodny, ale także ułatwia jego dalsze rozwijanie i utrzymanie. 

Pamiętajmy również o zasadzie TDD (Test Driven Development), która polega na pisaniu testów przed samym kodem. Dzięki temu mamy pewność, że nasz kod będzie zgodny z oczekiwaniami i spełni wszystkie wymagania, a testy staną się częścią naszego procesu pisania kodu.

## Zobacz też

- ["A Complete Guide to Writing Unit Tests in Swift" od Ray Wenderlich](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
- ["Introduction to Testing in Swift" od Apple](https://developer.apple.com/documentation/swift/testing)
- ["Why Testing Matters" od Hacking with Swift](https://www.hackingwithswift.com/articles/156/why-testing-matters)