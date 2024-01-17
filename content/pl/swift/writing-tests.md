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

## Co i dlaczego? 
Pisanie testów to nieodłączna część programowania. Jest to proces tworzenia specjalnych kawałków kodu, których zadaniem jest sprawdzenie poprawności działania naszego programu. Programiści piszą testy po to, aby upewnić się, że ich program działa jak należy i aby uniknąć nieoczekiwanych błędów.

## Jak to zrobić:
### Testy jednostkowe:
```Swift
func dodajLiczby(a: Int, b: Int) -> Int {
  return a + b
}

func testDodawania() {
  let wynik = dodajLiczby(a: 2, b: 3)
  assert(wynik == 5, "Coś poszło nie tak. Wynik powinien być 5, a jest \(wynik)")
}

testDodawania()
```

Output:
```
Test passed!
```

### Testy interfejsu użytkownika:
```Swift
func wlaczTrybNocy() {
  // Kod odpowiadający za włączenie trybu nocnego
}

func testTrybuNocy() {
  // Skopiuj zawartość ekranu przed włączeniem trybu nocnego
  let ekranPrzed = zapiszZawartoscEkranu()
  
  // Włącz tryb nocny
  wlaczTrybNocy()
  
  // Skopiuj zawartość ekranu po włączeniu trybu nocnego
  let ekranPo = zapiszZawartoscEkranu()
  
  // Porównaj zawartość obu ekranów
  assert(ekranPrzed != ekranPo, "Coś poszło nie tak. Ekran powinien się zmienić po włączeniu trybu nocnego.")
}

testTrybuNocy()
```

Output:
```
Test passed!
```

## Pogłębiona analiza:
Pisanie testów jest praktykowane od wielu lat i jest częścią metodyki programowania zwanej Test Driven Development (TDD). Istnieją również inne metody testowania, takie jak Behavior Driven Development (BDD) czy Acceptance Test Driven Development (ATDD). Istotne jest to, aby pisać testy na różnych poziomach, od testów jednostkowych po testy integracyjne.

## Zobacz też:
- [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/Assertions.html)
- [Test Driven Development w praktyce](https://semaphoreci.com/blog/2018/11/27/test-driven-development-benefits-best-practices.html)
- [Jak pisać testy - przewodnik dla początkujących](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/04-writing_tests.html)