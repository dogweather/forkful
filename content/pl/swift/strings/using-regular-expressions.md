---
aliases:
- /pl/swift/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:30.430938-07:00
description: "Wyra\u017Cenia regularne, czyli regex, to sekwencje znak\xF3w, kt\xF3\
  re tworz\u0105 wzorzec wyszukiwania, cz\u0119sto u\u017Cywane do zada\u0144 dopasowywania\
  \ lub manipulacji ci\u0105gami\u2026"
lastmod: 2024-02-18 23:08:49.943175
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne, czyli regex, to sekwencje znak\xF3w, kt\xF3re\
  \ tworz\u0105 wzorzec wyszukiwania, cz\u0119sto u\u017Cywane do zada\u0144 dopasowywania\
  \ lub manipulacji ci\u0105gami\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne, czyli regex, to sekwencje znaków, które tworzą wzorzec wyszukiwania, często używane do zadań dopasowywania lub manipulacji ciągami znaków. Programiści wykorzystują je do wszystkiego, począwszy od walidacji danych, analizy, po transformacje, czyniąc je niezbędnym narzędziem w przetwarzaniu i manipulacji tekstem w różnych językach programowania, w tym w Swift.

## Jak to zrobić:
Natywne wsparcie Swifta dla regex wykorzystuje klasę `NSRegularExpression`, razem z metodami zakresu i zastąpienia klasy String. Poniżej znajduje się przykład użycia regex, aby znaleźć i wyróżnić adresy e-mail w bloku tekstu:

```swift
import Foundation

let text = "Contact us at support@example.com or feedback@example.org for more information."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Znaleziono: \(text[range])")
        }
    } else {
        print("Nie znaleziono dopasowań.")
    }
} catch {
    print("Błąd regex: \(error.localizedDescription)")
}

// Przykładowe wyjście:
// Znaleziono: support@example.com
// Znaleziono: feedback@example.org
```

Dla bardziej złożonych lub bardziej wygodnych scenariuszy możesz użyć bibliotek stron trzecich, takich jak SwiftRegex, które upraszczają składnię i rozszerzają możliwości. Chociaż biblioteka standardowa Swifta jest potężna, niektórzy programiści preferują te biblioteki ze względu na ich zwięzłą składnię i dodatkowe funkcje. Oto jak można by wykonać podobne zadanie, używając hipotetycznej biblioteki stron trzecich:

```swift
// Zakładając, że istnieje biblioteka o nazwie SwiftRegex i jest zaimportowana
let text = "Reach out at hello@world.com or visit our website."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Hipotetyczna metoda dostarczona przez SwiftRegex
if emails.isEmpty {
    print("Nie znaleziono adresów email.")
} else {
    emails.forEach { email in
        print("Znaleziono: \(email)")
    }
}

// Hipotetyczne wyjście zakładając, że metoda `matches(for:)` istnieje w SwiftRegex:
// Znaleziono: hello@world.com
```

Przykład ten ilustruje, jak używać zewnętrznego pakietu wyrażeń regularnych do uproszczenia znajdowania dopasowań w ciągu znaków, zakładając, że istnieją takie wygodne metody jak `matches(for:)`. Ważne jest, aby odwołać się do odpowiedniej dokumentacji biblioteki stron trzecich, aby uzyskać dokładną składnię i dostępność metod.
