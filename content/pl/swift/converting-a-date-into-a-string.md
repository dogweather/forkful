---
title:                "Konwertowanie daty na ciąg znaków."
html_title:           "Swift: Konwertowanie daty na ciąg znaków."
simple_title:         "Konwertowanie daty na ciąg znaków."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja daty na string to proces przekształcania wartości daty (np. 31.12.2020) na postać tekstową (np. "31 grudnia 2020"). Programiści często wykonują tę operację w celu wyświetlenia daty w przystępniejszej formie dla użytkownika lub dla zapisania jej w bazie danych.

## Jak to zrobić:
W języku Swift istnieje wbudowany typ danych `Date`, który przechowuje datę i godzinę. Aby przekonwertować tę wartość na string, należy użyć metody `String(describing:)` i podać jako argument datę, którą chcemy przekonwertować. Poniżej znajduje się przykładowy kod wraz z wynikiem:

```Swift
let date = Date()
let dateString = String(describing: date)
print(dateString)

Wynik: "2020-06-15 16:32:41 +0000" 
```

Możesz również użyć klasy `DateFormatter` i dostosować format wyjściowy. Na przykład:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let date = Date()
let dateString = formatter.string(from: date)
print(dateString)

Wynik: "15/06/2020"
```

## Głębszy Zanurzenie:
Konwersja daty na string jest powszechnym zadaniem w programowaniu, ponieważ umożliwia wyświetlanie dat w różnych formatach, co jest przydatne dla użytkowników lub jako format danych w bazie. Istnieje również wiele innych metod konwersji daty, takich jak przekształcanie jej w timestamp. W języku Swift możesz również wybrać różne formaty językowe dla dat, dzięki czemu użytkownicy z różnych regionów mogą wyświetlać daty w odpowiednich dla siebie formatach.

## Zobacz również:
- [Oficjalna dokumentacja Swift na temat klasy Date](https://developer.apple.com/documentation/foundation/date)
- [Poradnik na temat konwersji daty na string w Swift](https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter)
- [Inne metody formatowania dat w języku Swift](https://www.raywenderlich.com/6742-tutorial-nsformatter-in-detail)