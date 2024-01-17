---
title:                "Pobieranie bieżącej daty"
html_title:           "Swift: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Pobranie aktualnej daty to proces, w którym programista uzyskuje bieżącą datę z systemu operacyjnego lub urządzenia. Jest to przydatne w celu pokazywania aktualnej daty i godziny użytkownikom lub do celów związanych z zapisywaniem i analizowaniem czasu w aplikacji.

## Jak to zrobić:
```Swift
let currentDate = Date()
print("Aktualna data to: \(currentDate)")
```

## Głębszy Zanurzenie:
Pobieranie aktualnej daty było jednym z pierwszych problemów, z którym musieli zmierzyć się programiści. W przeszłości wymagało to ręcznego wprowadzania daty, co mogło prowadzić do błędów. Alternatywnym rozwiązaniem jest używanie specjalnych bibliotek lub frameworków do pobierania daty, na przykład `NSCalendar` czy `Calendar` w Swift. W przypadku aplikacji mobilnych, aktualna data może również być pobierana z usługi sieciowej.

## Zobacz również:
- [Dokumentacja NSDate](https://developer.apple.com/documentation/foundation/nsdate)
- [Przewodnik po użytkowaniu daty i czasu w Swift](https://www.hackingwithswift.com/quick-start/swiftui/how-to-work-with-dates-and-times-in-swiftui)