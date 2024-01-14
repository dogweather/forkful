---
title:    "Swift: Porównywanie dwóch dat"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego?

Porównywanie dwóch dat może się wydawać prostym zadaniem, ale w rzeczywistości może stanowić wyzwanie dla wielu programistów Swift. W tym wpisie dowiesz się dlaczego warto nauczyć się porównywać daty w Swift oraz poznasz przykłady kodu, które pomogą Ci to zrobić.

## Jak to zrobić?

Porównywanie dwóch dat w Swift jest możliwe dzięki wykorzystaniu kilku dostępnych funkcji i metod. Przyjrzymy się temu na przykładzie dwóch dat typu Date. Przedstawione poniżej przykłady wykorzystują różne metody i zwracają wartość typu Bool, określającą wynik porównania dat.

```Swift
// Porównanie dwóch dat za pomocą funkcji "is after"
let date1 = Date(timeIntervalSinceReferenceDate: 0)
let date2 = Date(timeIntervalSinceReferenceDate: 86400) // 1 dzień po dacie1

print(date1.isAfter(date2)) // false
print(date2.isAfter(date1)) // true

// Porównanie dwóch dat za pomocą operatora "<"
let date3 = Date(timeIntervalSinceReferenceDate: 1614556800) // 1 marca 2021 r.
let date4 = Date(timeIntervalSinceReferenceDate: 1614609600) // 2 marca 2021 r.

print(date3 < date4) // true
print(date4 < date3) // false

// Porównanie dwóch dat za pomocą metody "compare"
let date5 = Date(timeIntervalSinceReferenceDate: 1614720000) // 3 marca 2021 r.
let date6 = Date(timeIntervalSinceReferenceDate: 1614643200) // 2 marca 2021 r.

print(date5.compare(date6)) // .orderedDescending
print(date6.compare(date5)) // .orderedAscending
```

W powyższych przykładach możemy zobaczyć, że wartością zwracaną jest typ Bool lub enum, który pozwala nam określić relację między porównywanymi datami.

## Głębszy zanurzenie

Porównywanie dat może być trudniejsze, gdy chcemy uwzględnić również czas i strefę czasową. W takich przypadkach warto skorzystać z klasy Calendar i jej metod do porównywania dat. Przykładowy kod poniżej porównuje dwie daty, biorąc pod uwagę również czas i strefę czasową.

```Swift
// Porównanie dat z uwzględnieniem czasu i strefy czasowej
let calendar = Calendar.current
let date7 = Date(timeIntervalSinceReferenceDate: 1614720000) // 3 marca 2021 r.
let date8 = Date(timeIntervalSinceReferenceDate: 1614643200) // 2 marca 2021 r.

let result = calendar.compare(date7, to: date8, toGranularity: .hour)
print(result) // .orderedDescending
```

Warto również zwrócić uwagę na to, że porównując daty, musimy uwzględnić różnice wynikające z różnych stref czasowych lub zastosować odpowiednie przekształcenia dat przed porównaniem.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o porównywaniu dat w Swift, polecamy zapoznanie się z dokumentacją języka Swift oraz artykułami na temat tego zagadnienia:

- [Dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/NSDate.html#ID134)
- [WWDC 2015 Session 24 - Bringing Modern Objective-C to Swift](https://developer.apple.com/videos/play/wwdc2015/402/)
- [Blog "Swift by Sundell" - Working with dates and time zones in Swift](https://www.swiftbysundell.com/tips/working-with-dates-and-time-zones-in-swift/)

Mamy nadzieję, że ten wpis okazał się dla Ciebie pomocny i pomoże Ci w lepszym zrozumieniu procesu porównywania dat w Swift.