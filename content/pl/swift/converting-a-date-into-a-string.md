---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Zamiana daty na ciąg znaków w Swift

## Co to i dlaczego?

Zamiana daty na ciąg znaków w Swift oznacza przekształcenie obiektu typu 'Date' w czytelny dla człowieka format, przy pomocy klasy DateFormatter. Programiści robią to, by uprościć wyświetlanie dat i godzin dla użytkowników.

## Jak to zrobić:

Poniżej znajduje się proste użycie DateFormatter w Swift:

```swift
let date = Date()
let dateFormatter = DateFormatter()

dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateInStringFormat = dateFormatter.string(from: date)

print(dateInStringFormat)
```
Wyjście:
```swift
2021-11-19 14:54:23
```
Proces jest prosty - tworzysz instancję DateFormatter, ustawiasz format, a następnie używasz metody 'string(from:)' by przekształcić datę na ciąg znaków.

## Przyjrzyjmy się bliżej:

Metoda przekształcania daty na ciąg znaków ma swoje korzenie w języku Objective-C, z którego wywodzi się Swift. Formatowanie daty było często używane w aplikacjach iOS, które musiały wyświetlać daty i czas w czytelnej formie.

Alternatywnie możemy używać ISO8601DateFormatter dla formatowania daty zgodnie z międzynarodowym standardem. Możemy również samodzielnie określać style dla daty i czasu, przy pomocy DateComponents.

```swift
let formatter = ISO8601DateFormatter()
let stringDate = formatter.string(from: Date())
print(stringDate)
```
Zaletą używania DateFormatter jest to, że obsługuje on lokalizację, co jest niezwykle przydatne w przypadku tworzenia aplikacji dla użytkowników z różnych krajów.

## Zobacz także:

1. [Dokumentacja Apple na temat DateFormattera](https://developer.apple.com/documentation/foundation/dateformatter)
2. [Dokumentacja Apple na temat ISO8601DateFormattera](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
3. [Jak używać Date, DateFormatter i DateComponents](https://www.hackingwithswift.com/articles/141/how-to-use-dates-and-time-in-swift)