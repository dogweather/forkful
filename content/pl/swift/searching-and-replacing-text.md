---
title:    "Swift: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym blogu omówimy ważną i często używaną funkcję w programowaniu Swift - wyszukiwanie i zamiana tekstu. Jest to niezbędne w wielu przypadkach, na przykład kiedy chcemy zmienić wszystkie wystąpienia pewnego słowa w naszym kodzie lub przetworzyć dane pobrane z zewnętrznego źródła. Pozwala to zaoszczędzić czas i wysiłek, a także uniknąć błędów ludzkich.

## Jak to zrobić

Aby przeprowadzić wyszukiwanie i zamianę tekstu w Swift, musimy użyć metody `replacingOccurrences(of:with:)` na obiekcie typu `String`. Spójrzmy na przykład:

```Swift
// Tworzenie przykładowego tekstu do zmiany
let text = "Cześć, jestem Swift i lubię jąc, jambu"

// Wyświetlenie oryginalnego tekstu
print(text) // wypisze "Cześć, jestem Swift i lubię jąc, jambu"

// Zamiana słowa "lubię" na "uwię"
let newText = text.replacingOccurrences(of: "lubię", with: "uwię")

// Wyświetlenie nowego tekstu
print(newText) // wypisze "Cześć, jestem Swift i uwię jąc, jambu"
```

W powyższym przykładzie użyliśmy metody `replacingOccurrences(of:with:)`, dostępnej na każdym obiekcie typu `String`, aby zamienić wszystkie wystąpienia słowa "lubię" na "uwię". Metoda ta przyjmuje dwa argumenty: słowo, które chcemy zamienić, oraz nowe słowo, na które chcemy je zamienić. 

## Głębsze wgląd

Wyszukiwanie i zmiana tekstu w Swift ma wiele możliwości. Możemy na przykład użyć metody `replacingOccurrences(of:with:options:range:)`, która pozwala określić opcje oraz zakres tekstu, na którym ma być dokonana zamiana. Możemy również użyć wyrażeń regularnych, aby odnaleźć i zmienić tekstu zawierające konkretny wzorzec. 

Istnieją także inne metody, które mogą pomóc w procesie wyszukiwania i zamiany tekstu, takie jak `replacingCharacters(in:with:)` czy `replacingOccurrences(of:, with:, options:, range:)`. Warto zapoznać się z dokumentacją i eksperymentować z różnymi metodami, aby znaleźć najlepsze rozwiązanie dla danego problemu.

## Zobacz także
- Dokumentacja Swift: [String.metody wyszukiwania i zamiany](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- Przykładowe zastosowania wyszukiwania i zamiany w Swift: [Searching and Replacing Text in Swift](https://medium.com/@toptensoftware/swift-searching-and-replacing-text-in-strings-32391b7f619d)