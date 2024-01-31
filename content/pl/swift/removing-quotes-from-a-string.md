---
title:                "Usuwanie cudzysłowów z ciągu znaków"
date:                  2024-01-26T03:42:33.974419-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie cudzysłowów z ciągu znaków oznacza wycięcie wszystkich znaków cudzysłowu, które otaczają treść. Robimy to, aby oczyścić dane wejściowe, przygotować dane do przechowywania lub pozbyć się niepotrzebnego formatowania tekstu, które może zakłócać przetwarzanie danych.

## Jak to zrobić:

Swift pozwala w dość wygodny sposób poradzić sobie z zadaniem usuwania cudzysłowów. Oto szybki przykład użycia `replacingOccurrences(of:with:)`, który robi dokładnie to, na co wygląda – zamienia fragmenty tekstu na coś innego lub nic.

```swift
var quotedString = "\"To jest 'cytowany' ciąg znaków.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // To jest 'cytowany' ciąg znaków.

// Radzenie sobie z pojedynczymi cudzysłowami? Wystarczy zmienić szukany ciąg.
quotedString = "'Oto inny przykład.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Oto inny przykład.
```

Wynikiem będą ciągi znaków bez cudzysłowów, gotowe na cokolwiek masz zaplanowane dalej.

## Dogłębna analiza

"Oczyszczamy" ciągi znaków w ten sposób od zarania programowania. W dawnych czasach chodziło bardziej o oszczędzanie cennej pamięci i unikanie błędów składni przy przetwarzaniu danych wejściowych. Przesuwając się do dzisiaj, chodzi o dobre zarządzanie danymi – szczególnie przy pracy z JSON-em lub przygotowywaniu ciągów znaków do pracy z bazą danych. Zgubiony cudzysłów może zepsuć zapytania SQL szybciej, niż można powiedzieć "błąd składni".

Alternatywy? Cóż, jeśli uważasz, że `replacingOccurrences(of:with:)` jest zbyt proste, możesz zagłębić się w wyrażenia regularne dla bardziej skomplikowanych wzorców lub gdy chcesz usuwać cudzysłowy tylko w określonych miejscach. Klasa `NSRegularExpression` w Swifcie jest tutaj Twoim przyjacielem. Ale pamiętaj, że regex to miecz obosieczny – potężny, ale czasami nadmierny.

Pod względem implementacji, `replacingOccurrences(of:with:)` to metoda dostarczana przez `String` w Swifcie, która wewnętrznie wywołuje bardziej skomplikowane funkcje manipulacji ciągami znaków, radzące sobie z Unicode i innymi zawiłościami nowoczesnego przetwarzania tekstu. To jedna z tych spraw "prosta na powierzchni, skomplikowana w środku", z którą Swift radzi sobie, abyś Ty nie musiał.

## Zobacz również

Aby dowiedzieć się więcej o manipulacji ciągami znaków w Swift:

- Język programowania Swift (Ciągi znaków i znaki): [Dokumentacja Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Dokumentacja dla programistów Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)

A jeśli teraz jesteś ciekaw wyrażeń regularnych i chcesz przetestować swoje wzorce:

- Regex101: [Tester i debuger wyrażeń regularnych](https://regex101.com)
