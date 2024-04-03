---
date: 2024-01-19
description: "How to: W Swift u\u017Cywamy `Codable` do \u0142atwego przetwarzania\
  \ JSON."
lastmod: '2024-03-13T22:44:35.775954-06:00'
model: unknown
summary: "W Swift u\u017Cywamy `Codable` do \u0142atwego przetwarzania JSON."
title: Praca z JSON
weight: 38
---

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
