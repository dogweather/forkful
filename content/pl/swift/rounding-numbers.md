---
title:                "Zaokrąglanie liczb"
date:                  2024-01-26T03:47:00.747181-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaokrąglanie liczb oznacza przybliżenie wartości numerycznej do określonej precyzji, zwykle w celu usunięcia niechcianych miejsc po przecinku. Programiści zaokrąglają, aby zarządzać pamięcią, poprawić czytelność oraz spełnić wymagania specyficzne dla danej dziedziny, takie jak ograniczenia walutowe.

## Jak to zrobić:

Swift oferuje kilka sposobów na zaokrąglenie liczb. Oto przedsmak:

```Swift
let original = 3.14159

// Standardowe zaokrąglanie
let standardRounded = round(original) // 3.0

// Zaokrąglanie do określonego miejsca po przecinku
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Zaokrąglanie w dół
let roundedDown = floor(original) // 3.0

// Zaokrąglanie w górę
let roundedUp = ceil(original) // 4.0

print("Standardowe: \(standardRounded), Dziesiętne: \(decimalRounded), W dół: \(roundedDown), W górę: \(roundedUp)")
```

Wynik: `Standardowe: 3.0, Dziesiętne: 3.142, W dół: 3.0, W górę: 4.0`

## Zanurzenie się głębiej

Historycznie, zaokrąglanie to matematyczne pojęcie starsze niż komputery, kluczowe w handlu i nauce. Framework `Foundation` Swift'a oferuje obszerne funkcjonalności zaokrąglania:

- `round(_: )` to dobre stare zaokrąglanie do najbliższej połowy w górę.
- `floor(_: )` i `ceil(_: )` obsługują zaokrąglanie kierunkowe.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` daje większą kontrolę z wykorzystaniem enum reguł zaokrąglania.

Bądź świadomy typu `Decimal` dla precyzyjnych obliczeń finansowych, który unika błędów związanych z arytmetyką zmiennoprzecinkową. Zbadaj również `NSDecimalNumber` dla kompatybilności z Objective-C.

## Zobacz również

- Standard IEEE dotyczący arytmetyki zmiennoprzecinkowej (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
