---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
YAML — це мова розмітки для конфігурації даних. Програмісти використовують його через легкість читання та інтеграцію з різноманітними мовами програмування.

## How to: (Як це зробити:)
Swift не має вбудованої підтримки YAML, але бібліотека Yams може допомогти.

```Swift
// Підключення Yams
import Yams

let yamlString = """
- name: Oleksiy
  job: Developer
- name: Iryna
  job: Designer
"""

// Десеріалізація YAML строки в Swift масив
if let people = try? Yams.load(yaml: yamlString) as? [[String: String]] {
    for person in people {
        print("\(person["name"] ?? "") is a \(person["job"] ?? "").")
    }
}
```

Друкує:
```
Oleksiy is a Developer.
Iryna is a Designer.
```

## Deep Dive (Поглиблений Розгляд):
YAML, випущений у 2001 році, побудований на основі XML і JSON. Альтернативи, такі як JSON і plist в Swift, також популярні для конфігурації. Yams реалізує що дозволяє здійснювати десеріалізацію та серіалізацію YAML даних.

## See Also (Дивіться також):
- [Yams GitHub репозиторій](https://github.com/jpsim/Yams)
- [Офіційна YAML специфікація](https://yaml.org/spec/1.2/spec.html)
- [Swift документація по plist (Property List файлам)](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PropertyLists/UnderstandXMLPlist/UnderstandXMLPlist.html)
