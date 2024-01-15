---
title:                "Praca z formatem json"
html_title:           "Swift: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest jednym z najczęściej używanych formatów do przechowywania i przesyłania danych w aplikacjach. Umiejętność pracy z nim jest niezbędna dla programistów aplikacji Swift, ponieważ pozwala na łatwe i szybkie pobieranie i przetwarzanie informacji z serwera. 

## Jak to zrobić

```Swift
let json = """
{
    "name": "John Doe",
    "age": 28
}
"""
if let jsonData = json.data(using: .utf8) {
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Name: \(user.name), Age: \(user.age)")
    } catch {
        print("Error decoding JSON: \(error)")
    }
}
```

Output: `Name: John Doe, Age: 28`

*Jeśli jeszcze nie masz doświadczenia w pracy z JSON w Swift, najlepszym miejscem do rozpoczęcia jest przeczytanie dokumentacji Apple na temat pracy z JSON w Swift i odwiedzenie forów programistycznych, gdzie możesz znaleźć wiele przydatnych wskazówek i przykładów.*

## Glebokie zanurzenie

Praca z JSON w Swift może być trochę skomplikowana, szczególnie przy dekodowaniu złożonych struktur danych. Warto zapoznać się z operatorem nil-coalescing (`??`) oraz funkcją mapowania (`map()`) jako przydatnymi narzędziami w przetwarzaniu danych JSON.

## Zobacz też

- [Dokumentacja Apple: Tworzenie i deserializacja JSON w Swift](https://developer.apple.com/documentation/foundation/archives_and_serialization/json)
- [Stack Overflow: Praca z JSON w Swift](https://stackoverflow.com/questions/43290529/parsing-json-data-in-swift)
- [Ray Wenderlich: Przewodnik po używaniu JSON w Swift](https://www.raywenderlich.com/121540/swift-tutorial-protocol-oriented-programming)