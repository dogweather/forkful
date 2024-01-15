---
title:                "Porównywanie dwóch dat"
html_title:           "Swift: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest częstą czynnością w programowaniu. Pozwala na ustalenie, która z dwóch dat jest wcześniejsza lub późniejsza, co jest przydatne w przypadku tworzenia aplikacji, które wymagają obsługi dat.

## Jak To Zrobić

Aby porównać dwie daty w języku Swift, możesz skorzystać z metody `compare` dostępnej na typie `Date`. Poniżej znajduje się przykład kodu, który porównuje dwie daty i wypisuje wynik w konsoli:

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) // tworzy datę późniejszą o 3600 sekund
let result = date1.compare(date2)

switch result {
case .orderedAscending:
    print("Data 1 jest wcześniejsza niż data 2")
case .orderedDescending:
    print("Data 2 jest wcześniejsza niż data 1")
case .orderedSame:
    print("Obie daty są takie same")
}
```

W powyższym przykładzie wykorzystano enum `ComparisonResult`, który może przyjmować jedną z trzech wartości: `.orderedAscending`, `.orderedDescending` lub `.orderedSame`. W zależności od wyniku porównania, wypisuje się odpowiedni komunikat w konsoli.

## Głębszy Wgląd

W języku Swift istnieją również inne sposoby porównywania dat, takie jak wykorzystanie operatorów większości lub mniejszości (`<`, `>`), lub skorzystanie z metody `compare(_:toGranularity:)`, która pozwala na porównanie dat z określoną dokładnością. W przypadku wykorzystania operatorów, typ `Date` zostanie automatycznie przekonwertowany na typ `Timeinterval`, co może dać nieoczekiwane wyniki.

Należy także pamiętać o uwzględnieniu strefy czasowej oraz obsłudze dat rozmiarów takich jak rok przestępny, ponieważ może to mieć wpływ na wynik porównania dwóch dat.

## Zobacz także

- [Dokumentacja Swift - Porównywanie dat](https://developer.apple.com/documentation/foundation/date)
- [Porównywanie dat w języku Swift](https://www.hackingwithswift.com/example-code/system/how-to-compare-dates)
- [Sposoby porównania dat w Swiftie](https://medium.com/@terence89/how-to-compare-dates-in-swift-4-6427222276f6)