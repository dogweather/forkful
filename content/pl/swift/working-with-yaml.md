---
title:                "Praca z formatem YAML"
html_title:           "Swift: Praca z formatem YAML"
simple_title:         "Praca z formatem YAML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co to jest YAML i po co to robią programiści?

YAML to format pliku używany do przechowywania i przesyłania danych. Programiści stosują go, ponieważ jest to sposób, w jaki mogą łatwo i czytelnie przechowywać różnego rodzaju informacje. Jest to szczególnie użyteczne, jeśli potrzebują one przechowywać dane w formacie tekstowym, a nie binarnym.

## Jak to zrobić?

```Swift
let yamlData = """
animal: cat
age: 6
favorite_food: tuna
"""

do {
    let object = try YAMLEncoder().encode(yamlData)
    print(object)
} catch {
    print("Błąd: \(error)")
}

// Output: ["age": 6, "favorite_food": "tuna", "animal": "cat"]
```

## Wnikliwe spojrzenie

Historia formatu YAML sięga 2002 roku, a jego nazwa jest akronimem od angielskich słów "YAML Ain't Markup Language". Alternatywą dla YAML jest JSON, który jest bardziej popularny, ale YAML ma bardziej czytelny i łatwiejszy do edycji format. W implementacji, YAML jest oparty na języku programowania Perl, ale jest dostępny także w innych językach, takich jak Swift.

## Zobacz także

- Oficjalna strona YAML: https://yaml.org/
- Więcej informacji o pracy z YAML w Swift: https://github.com/behrang/YamlSwift
- Przydatny przewodnik po składni YAML: https://www.tutorialspoint.com/yaml/