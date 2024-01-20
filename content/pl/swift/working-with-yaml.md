---
title:                "Praca z yaml"
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML to format danych popularny w konfiguracji i serializacji. Programiści używają go dla łatwości czytania i pisania, zwłaszcza w środowiskach DevOps i w projektach używających Docker oraz Kubernetes.

## Jak to zrobić:
Swift nie ma natywnej obsługi YAML, więc trzeba użyć zewnętrznej biblioteki, jak Yams. Aby zainstalować Yams, dodaj do `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

Kod do parsowania YAML:

```swift
import Yams

let yamlStr = """
name: Jan Kowalski
age: 30
languages:
  - Swift
  - Python
"""

do {
    if let person = try Yams.load(yaml: yamlStr) as? [String: Any] {
        print(person)
    }
} catch {
    print("Error parsing YAML: \(error)")
}
```

Sample output:

```
["name": "Jan Kowalski", "age": 30, "languages": ["Swift", "Python"]]
```

## Głębsze spojrzenie:
YAML, czyli "YAML Ain't Markup Language", jest bardziej zrozumiałym zamiennikiem dla XML i JSON. Jest używany od 2001 r. i dobrze współpracuje z różnymi językami programowania. Alternatywy dla YAML to JSON, TOML czy XML. Niektóre implementacje są ścieżką do ataków przez złożone struktury danych, dlatego jest ważne stosowanie zaufanych bibliotek.

## Zobacz też:
- Specyfikacja YAML: https://yaml.org/spec/
- Repozytorium GitHub Yams (Swift): https://github.com/jpsim/Yams
- Porównanie formatów konfiguracyjnych: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started