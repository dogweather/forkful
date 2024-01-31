---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:22.565176-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
YAML, что расшифровывается как "YAML Ain't Markup Language" (YAML - это не язык разметки), представляет собой стандарт сериализации данных, понятный человеку, который мы можем использовать для конфигурирования файлов или обмена данными. Программисты любят YAML за его простоту и читаемость, особенно в настройках конфигурации, сценариях CI/CD и системах оркестровки контейнеров.

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
