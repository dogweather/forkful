---
title:                "Swift: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Niespodziewane konieczności użycia daty w przyszłości lub przeszłości mogą się pojawić w wielu projektach programistycznych. Na przykład, może być konieczność utworzenia harmonogramu zadań lub obliczenia terminu ważności aplikacji. W tej blogowej publikacji zaprezentuję prosty sposób na wykorzystanie języka Swift do obliczenia daty w przyszłości lub przeszłości.

## Dlaczego

Często zdarza się, że w projekcie programistycznym potrzebujemy obliczyć datę w przyszłości lub przeszłości. Może to być potrzebne do ustawienia przypomnienia o ważnej dacie lub do zaplanowania działań na konkretny dzień. Dzięki temu prostemu sposobowi w języku Swift, możemy wykonać takie obliczenia w prosty i szybki sposób.

## Jak to zrobić

Do obliczenia daty w przyszłości lub przeszłości w języku Swift wykorzystamy klasę "Date" oraz operator "add" lub "subtract". W poniższym przykładzie wyliczymy datę, która jest 30 dni przed obecnym dniem:

```Swift
let today = Date()
let thirtyDaysAgo = today.addingTimeInterval(-30 * 24 * 60 * 60)
print("Trzydzieści dni temu było: \(thirtyDaysAgo)")
```
### Output:

```
Trzydzieści dni temu było: 2021-04-17 15:00:00 +0000
```
W powyższym przykładzie wykorzystaliśmy klasę "Date" do utworzenia obiektu reprezentującego dzisiejszą datę oraz operator "addingTimeInterval" do obliczenia daty z wykorzystaniem odpowiedniego przesunięcia w sekundach. W ten sposób możemy również obliczać daty w przyszłości poprzez zastosowanie operatora "add".

## Głębszy wgląd

W języku Swift mamy dostęp do wielu klas i metod, które ułatwiają pracę z datami. Klasa "Date" zawiera wiele metod, dzięki którym możemy manipulować datami. Dzięki temu możemy m.in. obliczać różnicę między dwoma datami, sprawdzać, czy dana data jest wcześniejsza czy późniejsza oraz konwertować daty na różne formaty.

Mamy również dostęp do klas takich jak "DateComponents" i "Calendar", które umożliwiają dokładniejsze manipulowanie datami i wykonanie bardziej skomplikowanych obliczeń. Warto zapoznać się z dokumentacją języka Swift, aby poznać wszystkie dostępne metody i możliwości związane z pracą z datami.

## Zobacz również

- [Dokumentacja języka Swift na temat pracy z datami](https://developer.apple.com/documentation/foundation/date)
- [Poradnik na temat obliczania daty w przyszłości lub przeszłości w języku Swift](https://www.hackingwithswift.com/example-code/system/how-to-add-days-to-a-date-using-calendar-and-datedatecomponents)
- [Blog z poradami dla początkujących programistów Swift](https://learnappmaking.com/beginners-guide-swift-programming/)