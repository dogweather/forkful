---
date: 2024-01-20 17:35:51.847539-07:00
description: "Jak to zrobi\u0107? \u0141\u0105czenie napis\xF3w to jedna z podstawowych\
  \ operacji. W j\u0119zykach takich jak C musieli\u015Bmy u\u017Cywa\u0107 funkcji\
  \ typu `strcat`, co by\u0142o mniej\u2026"
lastmod: '2024-04-05T22:50:50.084381-06:00'
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie napis\xF3w to jedna z podstawowych operacji."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

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
