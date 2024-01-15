---
title:                "Łączenie ciągów znaków"
html_title:           "Swift: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Konkatenacja, czyli łączenie ciągów tekstowych, jest podstawową czynnością w wielu programach. Dzięki temu możesz tworzyć nowe wyrażenia, wyświetlać użytkownikom dynamiczne komunikaty i bardziej elastycznie manipulować tekstem. W artykule tym dowiecie się, jak to zrobić w języku Swift.

## Jak to zrobić

Konkatenacja w Swift jest bardzo prosta. Aby połączyć dwa lub więcej ciągów tekstowych, wystarczy użyć operatora plus (+).

```Swift
let greet = "Cześć"
let name = "Maria"
let greeting = greet + " " + name

print(greeting)
// Output: Cześć Maria
```

Możesz także używać operatora plusa z przypisaniem (+=), aby dynamicznie dodawać kolejne fragmenty tekstu.

```Swift
var quote = "Życie jest jak jazda rowerem, żeby utrzymać równowagę, musisz iść do przodu, dodając "
let moreQuote = "świeżych wyzwań."

// Concatenate with assignment
quote += moreQuote

print(quote)
// Output: Życie jest jak jazda rowerem, żeby utrzymać równowagę, musisz iść do przodu, dodając świeżych wyzwań.
```

Warto również pamiętać, że w języku Swift ciągi tekstowe mogą być łączone bezpośrednio z innymi typami, takimi jak liczby czy znaki.

```Swift
let num1 = 3
let num2 = 5
let result = "Suma wynosi: " + String(num1 + num2)

print(result)
// Output: Suma wynosi: 8
```

## Głębszy zanurzenie

W języku Swift operatory do konkatenacji są wydajne i łatwe w użyciu, jednak warto zwrócić uwagę na pewne szczegóły. Na przykład, jeśli chcesz łączyć duże ilości ciągów tekstowych, lepiej zastosować metodę `append(_:)` dla klasy `String` zamiast używać operatora plusa. Jest to spowodowane tym, że operator plusa tworzy nowy ciąg tekstowy za każdym razem, gdy jest używany, co może spowolnić wykonywanie programu.

```Swift
let bigText = "Tekst 1, "
let text2 = "Tekst 2"
let result = bigText + text2

// This is not efficient - the whole string is copied each time
for _ in 1...1000 {
  result += text2
}

// This is more efficient as it appends the string without creating a new one each time
for _ in 1...1000 {
  result.append(text2)
}
```

## Zobacz również

Jeśli jesteś zainteresowany/a nauką języka Swift, zapoznaj się z naszymi innymi artykułami:

- [Tworzenie pętli w języku Swift](https://url.com)
- [Przetwarzanie danych typu JSON w Swift](https://url.com)
- [Podstawy deklarowania i używania funkcji w Swift](https://url.com)