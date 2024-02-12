---
title:                "Praca z YAML"
aliases:
- /pl/swift/working-with-yaml/
date:                  2024-02-03T19:27:03.553010-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML, co oznacza "YAML Ain't Markup Language", jest przyjaznym dla człowieka standardem serializacji danych dla wszystkich języków programowania. Programiści używają go do plików konfiguracyjnych, komunikacji międzyprocesowej i przechowywania danych, ponieważ jego czytelność jest znacznie bliższa zwykłemu językowi angielskiemu w porównaniu z innymi formatami danych, takimi jak XML czy JSON, co sprawia, że jest prostszy do zrozumienia i napisania.

## Jak to zrobić:
Swift nie zawiera wbudowanego wsparcia dla parsowania i serializacji YAML, co wymusza korzystanie z bibliotek stron trzecich. Popularnym wyborem jest `Yams`, biblioteka do pracy z YAML w Swift.

Najpierw, musisz dodać `Yams` do swojego projektu. Jeśli używasz Swift Package Managera, możesz dodać go jako zależność w pliku `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### Parsowanie YAML do Swift
Załóżmy, że masz następującą konfigurację YAML dla prostej aplikacji:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

Oto jak możesz sparsować ten ciąg YAML w Swift, używając `Yams`:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // Przykład dostępu do sparsowanych danych
        if let name = data["name"] as? String {
            print("Nazwa aplikacji: \(name)")
        }
    }
} catch {
    print("Błąd parsowania YAML: \(error)")
}
```

Przykładowe wyjście:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
Nazwa aplikacji: MyApp
```

### Serializacja obiektów Swift do YAML
Konwersja obiektu Swift z powrotem na ciąg YAML również jest prosta z `Yams`. Załóżmy, że masz tę samą strukturę danych, która musi być zserializowana:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("Błąd serializacji do YAML: \(error)")
}
```

To wyprodukuje ciąg sformatowany jako YAML:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

Te przykłady demonstrują podstawowe operacje pracy z YAML w aplikacjach Swift. Pamiętaj, że chociaż YAML wyróżnia się czytelnością dla człowieka i łatwością użycia, zawsze należy rozważyć specyficzne potrzeby twojej aplikacji, zwłaszcza w zakresie wydajności i złożoności, przy wyborze formatu serializacji danych.
