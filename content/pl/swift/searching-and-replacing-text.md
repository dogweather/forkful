---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Swift: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, programowanie jest nieodłączną częścią naszego życia. Musimy być w stanie szybko i sprawnie edytować nasze kody, aby sprostać wymaganiom projektów. Jednym z najczęstszych zadań jest wyszukiwanie i zamienianie tekstu, co pozwala nam na szybką i efektywną edycję naszych kodów.

## Jak to zrobić?

Sposobów na wyszukiwanie i zamienianie tekstu jest wiele, ale skupimy się na dwóch najprostszych i najczęściej używanych przez programistów.

### Metoda 1: Użycie funkcji `replacingOccurrences(of:with:)`

Ta funkcja jest dostępna dla typu String i pozwala na proste i szybkie wyszukiwanie i zamienianie tekstu.

```Swift
// Przykładowy tekst
let text = "Witajcie w świecie Swift!"

// Wyszukaj i zamień "Swift" na "Apple"
let newText = text.replacingOccurrences(of: "Swift", with: "Apple")

// Spodziewane wyjście: "Witajcie w świecie Apple!"
print(newText)
```

### Metoda 2: Użycie operatora `+=`

Ten operator pozwala na połączenie kilku tekstów w jedną zmienną i jednocześnie zamieniając określony tekst.

```Swift
// Przykładowe teksty
var text1 = "Witajcie w świecie"
let text2 = "Swift!"

// Połącz tekst i zamień "Swift" na "Apple"
text1 += text2.replacingOccurrences(of: "Swift", with: "Apple")

// Spodziewane wyjście: "Witajcie w świecie Apple!"
print(text1)
```

## Deep Dive

Podczas wyszukiwania i zamieniania tekstu, warto znać kilka dodatkowych funkcji, które ułatwią nam pracę.

### Opcje w funkcji `replacingOccurrences(of:with:options:range:)`

Ta funkcja ma dodatkowy parametr `options`, który pozwala na wybór różnych opcji wyszukiwania i zamieniania tekstu. Na przykład, możemy wybrać opcję `caseInsensitive`, aby ignorować wielkość liter podczas wyszukiwania i zamiany tekstu.

```Swift
// Przykładowy tekst
let text = "Witajcie w świecie Swift!"

// Wyszukaj i zamień "swift" na "Apple" bez względu na wielkość liter
let newText = text.replacingOccurrences(of: "swift", with: "Apple", options: .caseInsensitive)

// Spodziewane wyjście: "Witajcie w świecie Apple!"
print(newText)
```

### Użycie operatora `=` zamiast `+=`

Zamiast używać operatora `+=` do połączenia tekstów, możemy również użyć operatora `=` do bezpośredniej zamiany wartości zmiennej.

```Swift
// Przykładowe teksty
var text1 = "Witajcie w świecie"
let text2 = "Swift!"

// Połącz tekst i zamień "Swift" na "Apple" bez użycia operatora +=
text1 = text1.replacingOccurrences(of: "Swift", with: "Apple") + text2

// Spodziewane wyjście: "Witajcie w świecie Apple!"
print(text1)
```

## Zobacz również

- [Oficjalna dokumentacja Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial: Wyszukiwanie i zamienianie tekstu w Swift](https://www.hackingwithswift.com/example-code/strings/how-to-use-string-replacingoccurrences)
- [Stack Overflow: Jak zamienić część tekstu w Stringu Swift](https://stackoverflow.com/questions/24051738/how-do-i-replace-part-of-a-string-in-swift-language-i-e-a-substring)