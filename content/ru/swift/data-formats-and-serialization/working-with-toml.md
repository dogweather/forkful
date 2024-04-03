---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:05.725924-07:00
description: "TOML (\u042F\u0437\u044B\u043A \u0422\u043E\u043C\u0430, \u043E\u0447\
  \u0435\u0432\u0438\u0434\u043D\u044B\u0439 \u0438 \u043C\u0438\u043D\u0438\u043C\
  \u0430\u043B\u044C\u043D\u044B\u0439) \u2014 \u044D\u0442\u043E \u0444\u043E\u0440\
  \u043C\u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\
  \u0438 \u0434\u0430\u043D\u043D\u044B\u0445, \u043A\u043E\u0442\u043E\u0440\u044B\
  \u0439 \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0435\u0442\u0441\
  \u044F \u0431\u043B\u0430\u0433\u043E\u0434\u0430\u0440\u044F \u0441\u0432\u043E\
  \u0435\u0439 \u0447\u0435\u0442\u043A\u043E\u0439 \u0441\u0435\u043C\u0430\u043D\
  \u0442\u0438\u043A\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B\u2026"
lastmod: '2024-03-13T22:44:45.725084-06:00'
model: gpt-4-0125-preview
summary: "TOML (\u042F\u0437\u044B\u043A \u0422\u043E\u043C\u0430, \u043E\u0447\u0435\
  \u0432\u0438\u0434\u043D\u044B\u0439 \u0438 \u043C\u0438\u043D\u0438\u043C\u0430\
  \u043B\u044C\u043D\u044B\u0439) \u2014 \u044D\u0442\u043E \u0444\u043E\u0440\u043C\
  \u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438\
  \ \u0434\u0430\u043D\u043D\u044B\u0445, \u043A\u043E\u0442\u043E\u0440\u044B\u0439\
  \ \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0435\u0442\u0441\u044F\
  \ \u0431\u043B\u0430\u0433\u043E\u0434\u0430\u0440\u044F \u0441\u0432\u043E\u0435\
  \u0439 \u0447\u0435\u0442\u043A\u043E\u0439 \u0441\u0435\u043C\u0430\u043D\u0442\
  \u0438\u043A\u0435."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

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
