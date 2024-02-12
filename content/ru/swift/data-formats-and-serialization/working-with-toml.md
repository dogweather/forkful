---
title:                "Работа с TOML"
aliases:
- /ru/swift/working-with-toml.md
date:                  2024-01-29T00:05:05.725924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
TOML (Язык Тома, очевидный и минимальный) — это формат сериализации данных, который легко читается благодаря своей четкой семантике. Программисты используют TOML для файлов конфигурации, где ключевыми являются удобочитаемость для человека и простота анализа для машины.

## Как использовать:
Для начала вам нужен парсер TOML. В Swift нет встроенного, так что давайте использовать `TOMLDecoder`. Установите его через Swift Package Manager, а затем сериализуйте и десериализуйте TOML с легкостью.

```Swift
import TOMLDecoder

let tomlString = """
title = "Пример TOML"

[owner]
name = "Том Престон-Вернер"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Название: \(config.title), Владелец: \(config.owner.name), ДР: \(config.owner.dob)")
    } catch {
        print("Ошибка при разборе TOML: \(error)")
    }
}
```

Этот код выводит:
```
Название: Пример TOML, Владелец: Том Престон-Вернер, ДР: 1979-05-27 07:32:00 +0000
```

## Погружение
TOML был разработан Томом Престон-Вернером, сооснователем GitHub, как более удобная для человека альтернатива форматам, таким как JSON или YAML. Он стремится к ясности, уменьшая шансы на неправильное толкование как человеком, так и машиной. Что касается альтернатив, то обычными подозреваемыми являются YAML и JSON, причем YAML ориентирован на читаемость для человека, а JSON — проще для понимания машиной. При работе с TOML в Swift у нас нет собственного парсера. Однако сторонние библиотеки, такие как `TOMLDecoder`, облегчают конвертацию между строками TOML и типами Swift, в особенности с помощью протоколов `Codable`, введенных в Swift 4, которые упростили сериализацию.

## Смотрите также
- Стандарт TOML: https://toml.io
- GitHub для `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Документация Swift по `Codable`: https://developer.apple.com/documentation/swift/codable
- Сравнение форматов сериализации данных: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
