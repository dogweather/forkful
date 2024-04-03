---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:22.565176-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: Swift \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\
  \u043E \u043D\u0435 \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\
  \u0435\u0442 YAML, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u043D\u0430\u043C\
  \ \u043D\u0443\u0436\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u0442\u044C \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044E\u044E\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443, \u0442\u0430\u043A\
  \u0443\u044E \u043A\u0430\u043A Yams. \u0421\u043D\u0430\u0447\u0430\u043B\u0430\
  \ \u0434\u043E\u0431\u0430\u0432\u044C\u0442\u0435 Yams \u0432 \u0432\u0430\u0448\
  \u2026"
lastmod: '2024-03-13T22:44:45.719976-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E \u043D\
  \u0435 \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0435\u0442\
  \ YAML, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u043D\u0430\u043C \u043D\u0443\
  \u0436\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\
  \u044C \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044E\u044E \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0443, \u0442\u0430\u043A\u0443\u044E \u043A\
  \u0430\u043A Yams."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как использовать:
Swift изначально не обрабатывает YAML, поэтому нам нужно использовать стороннюю библиотеку, такую как Yams. Сначала добавьте Yams в ваш `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

Затем, импортируйте Yams и используйте его для парсинга YAML в словарь Swift:

```swift
import Yams

let yamlString = """
name: John Doe
age: 34
languages:
  - Swift
  - Python
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
    }
} catch {
    print("Не удалось разобрать строку YAML.")
}

// Вывод:
// ["name": "John Doe", "age": 34, "languages": ["Swift", "Python"]]
```

Если вы хотите генерировать YAML из объектов Swift:

```swift
import Yams

let dictionary: [String: Any] = [
    "name": "Jane Smith",
    "age": 28,
    "languages": ["Java", "Kotlin"]
]

do {
    let yaml = try Yams.dump(object: dictionary)
    print(yaml)
} catch {
    print("Не удалось преобразовать словарь в YAML.")
}

// Вывод:
// age: 28
// languages:
//   - Java
//   - Kotlin
// name: Jane Smith
```

## Глубже
YAML появился в 2001 году как альтернатива XML, удобная для человека. Он напоминает JSON с меньшим использованием скобок и лучшей читаемостью для человека. В то время как для веб-API обычно выбирают JSON, YAML предпочитают для файлов конфигурации. Альтернативами являются TOML и JSON5, но использование пробельных символов в YAML и возможность комментирования строк делают его желательным. С Yams, Swift подходит к обработке YAML с классовым маппингом, предлагая баланс между простотой сценариев и типовой безопасностью.

## Смотрите также
- Официальный сайт YAML для деталей спецификации: [https://yaml.org](https://yaml.org)
- Репозиторий Yams на GitHub: [https://github.com/jpsim/Yams](https://github.com/jpsim/Yams)
- Документация Менеджера Пакетов Swift: [https://swift.org/package-manager/](https://swift.org/package-manager/)
