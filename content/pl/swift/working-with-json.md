---
title:                "Swift: Praca z json"
simple_title:         "Praca z json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Swift, na pewno słyszałeś już o JSON. Jest to jedna z najpopularniejszych form zapisywania danych w formacie tekstowym. Dzięki swojej prostocie i czytelności, JSON jest często wybierany do przechowywania i udostępniania danych, szczególnie w aplikacjach mobilnych. W tym artykule dowiesz się, dlaczego warto poznać ten format i jak zacząć z nim pracować.

## Jak

Do pracy z JSON w Swift potrzebujemy jedynie kilku prostych narzędzi. Po pierwsze, musimy importować moduł `Foundation` aby móc używać klasy `JSONSerialization`. Następnie, możemy wykorzystać funkcję `data(withJSONObject:options:)` aby przekonwertować dane z formatu JSON na obiekt typu `Data`. Aby zamienić dane z formatu JSON na klasę lub strukturę w Swift, możemy użyć funkcji `JSONDecoder`. Poniżej znajdują się przykładowe kody wraz z odpowiednimi komentarzami:

```Swift
import Foundation // importowanie modułu Foundation

// konwertowanie danych na format JSON z wykorzystaniem klasy JSONSerialization
// opcja prettyPrinted dodaje wcięcia i nowe linie dla lepszej czytelności
let jsonData = try JSONSerialization.data(withJSONObject: data, options: .prettyPrinted)

// konwertowanie danych z formatu JSON na strukturę lub klasę w Swift
let decodedData = try JSONDecoder().decode(StructOrClass.self, from: jsonData)
```

## Deep Dive

JSON jest bardzo prostym formatem, jednak może być również bardzo rozbudowany i skomplikowany. W przypadku bardziej zaawansowanych zastosowań, warto zapoznać się z różnymi opcjami konwersji danych z formatu JSON na obiekty w Swift, takimi jak `JSONSerialization.WritingOptions` i `JSONDecoder.KeyDecodingStrategy`. Warto również pamiętać o sposobie działania tych funkcji na typach opcjonalnych oraz obsłudze błędów.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o pracy z JSON w Swift, polecam zapoznać się z poniższymi źródłami:

- [Oficjalna dokumentacja Apple na temat pracy z JSON](https://developer.apple.com/documentation/foundation/jsonencoder)
- [Tutorial "Praca z JSON w Swift" na stronie Ray Wenderlich](https://www.raywenderlich.com/1797/working-with-json-in-swift-tutorial)
- [Poradnik "Podstawy pracy z JSON w Swift" na stronie Hacking with Swift](https://www.hackingwithswift.com/example-code/system/how-to-parse-json-using-jsonserialization)