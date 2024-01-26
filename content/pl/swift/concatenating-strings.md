---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:35:51.847539-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Łączenie napisów to po prostu sklejanie ich w jeden. Programiści robią to, żeby tworzyć sensowne komunikaty lub dynamicznie budować tekst.

## Jak to zrobić?
```Swift
let greeting = "Cześć, "
let name = "Janek!"
let welcomeMessage = greeting + name
print(welcomeMessage)
// Output: Cześć, Janek!
```

Interpolacja napisów:
```Swift
let apples = 3
let summary = "Mam \(apples) jabłka."
print(summary)
// Output: Mam 3 jabłka.
```

Dołączanie napisu:
```Swift
var order = "Chcę zamówić:"
order += " burgera"
print(order)
// Output: Chcę zamówić: burgera
```

## W Głąb Tematu
Łączenie napisów to jedna z podstawowych operacji. W językach takich jak C musieliśmy używać funkcji typu `strcat`, co było mniej intuicyjne i bezpieczne. Swift pozwala na prostą concatenation, dzięki czemu kod jest czytelny i odporny na błędy.

Alternatywy to interpolacja napisów, która jest często wygodniejsza i umożliwia wstawianie zmiennych bezpośrednio w tekst. Swift używa znaku "\\" do interpolacji, co jest wzorem przyjętym także w innych nowoczesnych językach.

Szczegół implementacyjny: W Swift, napisy to struktury, nie obiekty. Dzięki temu są szybsze i bardziej przewidywalne co do wydajności niż w językach, gdzie są one obiektami (`NSString` w Objective-C).

## Zobacz Również
- [String Interpolation w Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Apple’s Swift API Dla Napisów](https://developer.apple.com/documentation/swift/string)
- [Przewodnik po Swift od Apple](https://docs.swift.org/swift-book/)
