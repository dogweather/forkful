---
title:                "Refaktoryzacja"
aliases:
- pl/swift/refactoring.md
date:                  2024-01-26T03:37:59.381428-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces restrukturyzacji istniejącego kodu komputerowego bez zmiany jego zewnętrznego zachowania. Programiści robią to, aby oczyścić bazę kodu, poprawić czytelność, możliwość utrzymania oraz przygotować podłoże pod przyszłe funkcje z minimalnym długiem technicznym.

## Jak to zrobić:
Zacznijmy od podstawowego przykładu w Swift, gdzie mamy pewien powtarzalny kod:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Imię: \(firstName)")
    print("Nazwisko: \(lastName)")
    print("Wiek: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Stanowisko: \(title)")
    print("Firma: \(company)")
}
```

Refaktoryzacja tego obejmowałaby utworzenie struktury `User` do enkapsulacji atrybutów użytkownika oraz dodanie metody do drukowania szczegółów:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Imię: \(firstName)")
        print("Nazwisko: \(lastName)")
        print("Wiek: \(age)")
        print("Stanowisko: \(jobTitle)")
        print("Firma: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Developer Oprogramowania", company: "Tech Solutions")
user.printDetails()
```

### Przykładowe wyjście:
```
Imię: John
Nazwisko: Doe
Wiek: 30
Stanowisko: Developer Oprogramowania
Firma: Tech Solutions
```

## Szczegółowe omówienie
Refaktoryzacja ma swoje korzenie sięgające wczesnych dni inżynierii oprogramowania, ale termin ten zyskał na popularności pod koniec lat 90., szczególnie dzięki fundamentalnej książce Martina Fowlera "Refaktoryzacja: Ulepszanie struktury istniejącego kodu". Książka ta przedstawiła zasadę, że kod powinien być ciągle oczyszczany w małych krokach, zamiast czekać na oddzielną fazę.

Alternatywy dla ręcznej refaktoryzacji obejmują automatyczne narzędzia i środowiska IDE (Zintegrowane Środowisko Programistyczne), które mogą pomóc wykryć zduplikowany kod, sugerować uproszczenia i automatycznie generować części kodu. Xcode, na potrzeby rozwoju Swifta, oferuje różne narzędzia refaktoryzacyjne, takie jak zmiana nazwy i funkcjonalność wyodrębniania metody, które mogą zmniejszyć potencjał ludzkiego błędu w procesie.

Podczas implementacji refaktoryzacji ważne jest, aby mieć solidną suitę testów. Testy działają jak siatka bezpieczeństwa, zapewniając, że dokonane zmiany nie wprowadzają błędów. Jest to istotne, ponieważ głównym celem refaktoryzacji jest zmiana wewnętrznej struktury bez wpływu na zewnętrzne zachowanie.

## Zobacz także
- [„Refaktoryzacja: Ulepszanie struktury istniejącego kodu” autorstwa Martina Fowlera](http://martinfowler.com/books/refactoring.html)
- [Dokumentacja Swifta autorstwa Apple](https://swift.org/documentation/)
- [Korzystanie z narzędzi refaktoryzacji Xcode](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Przewodnik po stylu Swifta autorstwa Raya Wenderlicha](https://github.com/raywenderlich/swift-style-guide)
