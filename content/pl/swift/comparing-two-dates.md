---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Porównywanie dwóch dat to proces ustalania, która data następuje po innej. Programiści robią to aby przetwarzać i manipulować danymi datami w swoich aplikacjach.

## Jak to zrobić:
Tu jest łatwy przykład na porównanie dwóch dat w Swift:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let firstDate = dateFormatter.date(from: "24/12/2022")!
let secondDate = dateFormatter.date(from: "01/01/2023")!

if firstDate < secondDate {
    print("Pierwsza data jest wcześniejsza")
} else if firstDate > secondDate {
    print("Druga data jest wcześniejsza")
} else {
    print("Daty są takie same")
}
```
Kod powyżej generuje następujący rezultat:

```
Pierwsza data jest wcześniejsza
```

## Dogłębne spojrzenie
Mit porównania dat ma długą historię, zwłaszcza w zakresie zarządzania czasem i planowania zdarzeń. W przeszłości wykonanie takiego zadania było nieco trudne i wymagało zrozumienia tajników zarządzania czasem. 

Alternatywą dla porównania dat jest użycie specjalnych bibliotek, jak SwiftDate, które oferują wiele przydatnych funkcji dla dat i czasu. 

Co więcej, porządek, w jakim używamy operatorów porównania jest kluczowy. Zawsze używamy dwóch dat, które chcemy porównać koło siebie, używając operatora `<` albo `>` pomiędzy nimi.

## Zobacz też
Sprawdź te dodatkowe źródła, aby dowiedzieć się więcej:

1. Dokumentacja Apple na temat `Dates and Times` https://developer.apple.com/documentation/foundation/dates_and_times
2. Oficjalne tutoriale Apple's Swift https://docs.swift.org/swift-book/LanguageGuide/DatesAndTimes.html
3. Używanie SwiftDate do łatwego zarządzania datami: https://malcommac.github.io/SwiftDate/