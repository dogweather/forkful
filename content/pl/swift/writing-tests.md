---
title:                "Pisanie testów"
html_title:           "Swift: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego?

Testowanie kodu jest ważną częścią procesu programowania. Pozwala ono upewnić się, że nasz kod działa poprawnie i nie wprowadza błędów. Dzięki testom możemy mieć pewność, że nasza aplikacja będzie działać zgodnie z oczekiwaniami, a także ułatwić sobie późniejsze wprowadzanie zmian czy dodawanie nowych funkcjonalności.

## Jak to zrobić?

Najważniejszym elementem w testowaniu kodu w języku Swift są asercje (ang. assertions), które sprawdzają, czy dane zachowują się zgodnie z oczekiwaniami. Przykładowa asercja może wyglądać tak:

```Swift
XCTAssertEqual(2+2, 4)
```
W tym przypadku asercja sprawdza, czy wynik dodawania dwóch liczb jest równy 4. Jeśli tak, to test przechodzi, jeśli nie - wyświetlony zostaje komunikat o błędzie.

Możemy również testować funkcje i metody naszego kodu. Przykładowo, gdybyśmy mieli funkcję dodającą wartości z dwóch tablic, test może wyglądać tak:

```Swift
func addArrays(array1: [Int], array2: [Int]) -> Int {
    var sum = 0
    for value in array1 {
        sum += value
    }
    for value in array2 {
        sum += value
    }
    return sum
}
```
Aby przetestować tę funkcję, napiszemy:

```Swift
XCTAssertEqual(addArrays(array1: [1, 2, 3], array2: [4, 5, 6]), 21)
```
W ten sposób sprawdzamy, czy funkcja dodaje poprawnie liczby z dwóch tablic, a oczekiwany wynik to 21.

## Dogłębna analiza

Testowanie kodu jest ważne nie tylko dla poprawnej funkcjonalności naszej aplikacji, ale również dla wygody programistów. Dzięki testom możemy szybko i łatwo wyłapywać błędy i uniknąć ewentualnych kłopotów w przyszłości.

Istnieje wiele różnych rodzajów testów w języku Swift, takich jak testy jednostkowe, integracyjne czy UI. Każdy z nich ma swoje zalety i powinien być wykorzystywany w odpowiednich sytuacjach.

Jeśli chcesz dowiedzieć się więcej o testowaniu w języku Swift, polecamy Ci zapoznać się z artykułami na poniższych stronach:

- https://www.hackingwithswift.com/articles/148/unit-testing-for-swift-developers
- https://www.swiftbysundell.com/basics/unit-testing/

## Zobacz również

- Przewodnik dla początkujących: https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/01-introduction.html
- Dokumentacja Xcode: https://developer.apple.com/documentation/xctest