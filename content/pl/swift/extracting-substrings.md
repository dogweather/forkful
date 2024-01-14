---
title:                "Swift: Wydobywanie podciągów"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Podział ciągów znaków, zwany również rozdzielaniem substrings, jest podstawowym elementem programowania w języku Swift. Jest to przydatne narzędzie, które pozwala na wyodrębnienie określonych części tekstu, co może być niezbędne w wielu projektach.

## Jak to zrobić?

Aby wyodrębnić substring, należy skorzystać z metody ```Swift substring (from: )```. Poniżej znajduje się przykładowy kod, który wyjaśnia jak to zrobić w praktyce:

```Swift
let message = "Witaj na moim blogu!"

let start = message.index(message.startIndex, offsetBy: 6)
let end = message.index(message.endIndex, offsetBy: -7)

let substring = message.substring(from: start, to: end)
print(substring) // wynik: "na moim blogu"
```

## Deep Dive

Rozdzielanie substrings może być również użyteczne w przypadku pracy z tablicami i pętlami. Możemy na przykład wyodrębnić pewną część elementów z tablicy i przechowywać je w postaci substrings. Jest to szczególnie przydatne, gdy chcemy wykonać operacje na całych fragmentach tekstu bez konieczności przejmowania się pozostałymi częściami.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o podziale substrings, polecam przeczytać te artykuły:

- [Podstawy języka Swift](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-ID292)
- [Przykłady użycia metody substring w Swift](https://www.hackingwithswift.com/read/0/1/using-substrings-in-swift)
- [Rozdzielanie substrings w praktyce](https://www.raywenderlich.com/163857/working-strings-swift-3-0#toc-anchor-001)

Dziękujemy za przeczytanie naszego bloga! Do następnego razu!