---
title:                "Swift: Konwersja daty do ciągu znaków."
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w naszym programowaniu potrzebujemy zamienić datę na napis, na przykład aby wyświetlić ją użytkownikowi lub zapisać w bazie danych. W tym artykule dowiesz się jak to zrobić w języku Swift.

## Jak to zrobić

Aby zamienić datę na napis (string) w Swift, używamy klasy DateFormatter. Poniżej znajdziesz przykładowy kod:

```Swift
let date = Date()

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd MMMM yyyy"

let dateString = dateFormatter.string(from: date)
print(dateString) // Output: 14 listopada 2020
```

W tym przykładzie najpierw tworzymy obiekt Date, reprezentujący aktualną datę. Następnie tworzymy obiekt DateFormatter oraz ustawiamy odpowiedni format dla naszego napisu (w tym przypadku "dd MMMM yyyy" oznacza dzień miesiąca, nazwę miesiąca oraz rok). Na koniec, wywołujemy metodę string(from:) obiektu DateFormatter dla naszej daty, co zwróci nam napis w oczekiwanym formacie.

## Deep Dive

Jeśli chcesz dowiedzieć się więcej o konwersji daty na napis w Swift, możesz zapoznać się z oficjalną dokumentacją języka oraz przeczytać o różnych formatowaniach, które można użyć w DateFormatter. Istnieje również możliwość ustawiania własnych, niestandardowych formatów.

## Zobacz także

- [Oficjalna dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Dokumentacja klasy DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Przykładowe formaty daty w Swift](https://nsdateformatter.com)