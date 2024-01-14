---
title:    "Swift: Wyszukiwanie i zamienianie tekstu"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu nieodłącznym elementem jest edycja tekstu w kodzie. Często musimy zmienić pewne wyrazy, nazwy lub parametry w naszym kodzie. W takich sytuacjach bardzo przydatne jest umiejętne korzystanie z techniki "search and replace", czyli wyszukiwania i zamiany tekstu. Dzięki temu możemy szybko i skutecznie wprowadzić zmiany w naszym kodzie, oszczędzając cenny czas i unikając błędów.

## Jak to zrobić?

W języku Swift istnieją dwa główne sposoby na wyszukiwanie i zamianę tekstu: `replacingOccurrences(of:with:options:)` oraz `replacingCharacters(in:with:options:)`. Pierwsza metoda pozwala nam na zamianę jednego ciągu znaków na inny w całym tekście, natomiast druga umożliwia nam zamianę tylko części tekstu, wybranego za pomocą zakresu indeksów. Przykładowe użycie tych metod wyglądałoby następująco:

```Swift
let text = "Hello, World!"
let newText = text.replacingOccurrences(of: "Hello", with: "Hola")
print(newText) // Wypisze "Hola, World!"

let range = text.index(text.startIndex, offsetBy: 7) ..< text.endIndex
let anotherNewText = text.replacingCharacters(in: range, with: "Everyone")
print(anotherNewText) // Wypisze "Hello, Everyone!"
```

W obu przypadkach możemy również skorzystać z różnych opcji, takich jak ignorowanie wielkości liter czy ustawianie zakresu, w którym ma być wykonywana zamiana.

## Głębsze spojrzenie

W przypadku bardziej złożonych operacji związanych ze zmianą tekstu, możemy skorzystać z polecenia `enumerateSubstrings(in:options:_:)`. Jest to metoda, która umożliwia nam iterowanie po podciągach danego tekstu zgodnie z wybranymi opcjami. Dzięki temu możemy dokładnie kontrolować, które części tekstu chcemy zmienić, a które pozostawić bez zmian.

```Swift
let text = "Lorem ipsum dolor sit amet"
var newText = ""

text.enumerateSubstrings(in: text.startIndex..., options: .byWords) { (substring, range, _, _) in
    if let word = substring {
        if word.starts(with: "i") { // Jeśli wyraz zaczyna się od "i"
            newText.append("I") // Zamień na "I"
        } else {
            newText.append(word) // W przeciwnym razie pozostaw bez zmian
        }
        newText.append(" ")
    }
}

print(newText) // Wypisze "Lorem Ipsum dolor sit amet"
```

Teraz możemy łatwo dokonać zmiany tylko na wybranych częściach tekstu, nie naruszając reszty.

## Zobacz również

- [Dokumentacja Apple - Polecenie `replacingOccurrences(of:with:options:)`](https://developer.apple.com/documentation/foundation/nsstring/1414175-replacingoccurrences)
- [Dokumentacja Apple - Polecenie `enumerateSubstrings(in:options:_:)`](https://developer.apple.com/documentation/foundation/nsstring/1412741-enumeratesubstrings)
- [Dokumentacja Apple - Typ `String`](https://developer.apple.com/documentation/swift/string)