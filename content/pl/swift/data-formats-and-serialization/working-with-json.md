---
title:                "Praca z JSON"
aliases:
- /pl/swift/working-with-json.md
date:                  2024-01-19
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Praca z JSON (JavaScript Object Notation) to podstawa dla wymiany danych w aplikacjach. Programiści używają go, żeby łatwo serwisy mogły się "dogadywać" z różnymi platformami i językami.

## How to:
W Swift używamy `Codable` do łatwego przetwarzania JSON.

```Swift
import Foundation

// Definiowanie modelu
struct User: Codable {
    var name: String
    var age: Int
}

// Przykładowy JSON
let json = """
{
    "name": "Jan",
    "age": 28
}
""".data(using: .utf8)!

// Dekodowanie JSON do modelu
do {
    let user = try JSONDecoder().decode(User.self, from: json)
    print(user) // Output: User(name: "Jan", age: 28)
} catch {
    print(error)
}
```

## Deep Dive
JSON istnieje od 2001 roku, stworzony przez Douglasa Crockforda. Jest prostszy niż XML i da się go łatwo mapować na struktury danych w większości języków programowania. Alternatywy dla JSON to XML, YAML czy BSON, ale JSON wygrywa popularnością dzięki swej prostocie. W Swift, od wersji 4, `Codable` to standard do enkodowania i dekodowania danych, który zamienia obiekty Swift w JSON i na odwrót, wykorzystując `JSONEncoder` i `JSONDecoder`.

## See Also
- Oficjalna dokumentacja JSON: https://www.json.org/json-en.html
- Dokumentacja Swift Codable: https://developer.apple.com/documentation/swift/codable
- Porównanie formatów danych JSON, XML, YAML, BSON: https://www.oreilly.com/library/view/java-soa-cookbook/9780596802271/ch04.html
