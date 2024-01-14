---
title:    "Swift: Konwersja daty na ciąg znaków"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na łańcuch znaków jest nieodzownym krokiem w wielu aplikacjach Swift. Może to pomóc w wyświetlaniu daty w określonym formacie lub późniejszym przetwarzaniu jej jako danych tekstowych. Dzięki temu procesowi możesz dostosować wygląd i wykorzystanie dat w swojej aplikacji.

## Jak to zrobić

Aby przekonwertować datę na łańcuch znaków w Swift, używamy metody `string(from: Date)` w klasie `DateFormatter`. Najpierw tworzymy instancję `DateFormatter` i ustawiamy pożądany format daty za pomocą właściwości `dateFormat`. Następnie wywołujemy metodę `string(from: Date)` i przekazujemy jej obiekt `Date`, który chcemy przekonwertować. Oto przykładowy kod:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd MMMM yyyy" // format daty
let date = Date() // aktualna data
let dateString = dateFormatter.string(from: date)
print(dateString) // wyświetli "17 listopada 2021"
```

## Głębsze zagadnienia

Metoda `string(from: Date)` może być również przydatna, gdy chcemy przetworzyć datę z innego języka na format lokalny. W tym przypadku musimy ustawić odpowiedni locale w naszym `DateFormatter`. Na przykład, aby wyświetlić datę w języku polskim, możemy użyć:

```Swift
dateFormatter.locale = Locale(identifier: "pl_PL")
print(dateFormatter.string(from: date)) // wyświetli "17 listopada 2021"
```

Możesz również dostosować ustawienia dotyczące czasu, strefy czasowej i innych szczegółów dotyczących daty za pomocą odpowiednich właściwości w `DateFormatter`. Warto również przeczytać dokumentację dotyczącą tej klasy, aby poznać wszystkie dostępne opcje.

## Zobacz także

- [Dokumentacja Apple: Klasa DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift by Sundell: Working with dates in Swift](https://www.swiftbysundell.com/articles/working-with-dates-in-swift/)
- [Medium: Date, DateFormatter, Locale, i DateFormatterStyle — szybki przegląd](https://medium.com/@tugger3/swift-date-dateformatter-locale-dateformatterstyle-a-quick-overview-1c20f328aa2d)