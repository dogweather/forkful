---
date: 2024-01-20 17:35:51.847539-07:00
description: "\u0141\u0105czenie napis\xF3w to po prostu sklejanie ich w jeden. Programi\u015B\
  ci robi\u0105 to, \u017Ceby tworzy\u0107 sensowne komunikaty lub dynamicznie budowa\u0107\
  \ tekst."
lastmod: '2024-03-13T22:44:35.746822-06:00'
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie napis\xF3w to po prostu sklejanie ich w jeden. Programi\u015B\
  ci robi\u0105 to, \u017Ceby tworzy\u0107 sensowne komunikaty lub dynamicznie budowa\u0107\
  \ tekst."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
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
