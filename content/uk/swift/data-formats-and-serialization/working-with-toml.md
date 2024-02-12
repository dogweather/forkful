---
title:                "Робота з TOML"
aliases:
- /uk/swift/working-with-toml.md
date:                  2024-01-26T04:27:16.138259-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-toml.md"
---

{{< edit_this_page >}}

## Що та Чому?
TOML (Tom's Obvious, Minimal Language - Очевидна та мінімалістична мова Тома) - це формат серіалізації даних, який легко читати завдяки своїй чіткій семантиці. Програмісти використовують TOML для файлів конфігурації, де ключовими є зручність читання людьми та простота розбору машинами.

## Як це зробити:
Для початку вам потрібен парсер TOML. Swift не має вбудованого, тому використаємо `TOMLDecoder`. Встановіть його через Swift Package Manager, а потім з легкістю серіалізуйте та десеріалізуйте TOML.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
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
        print("Назва: \(config.title), Власник: \(config.owner.name), ДН: \(config.owner.dob)")
    } catch {
        print("Помилка парсингу TOML: \(error)")
    }
}
```

Цей код виводить:
```
Назва: TOML Example, Власник: Tom Preston-Werner, ДН: 1979-05-27 07:32:00 +0000
```

## Поглиблено
TOML було розроблено Томом Престон-Вернером, співзасновником GitHub, як більш дружню до людини альтернативу форматам на кшталт JSON чи YAML. Його мета - чіткість, зменшення шансів на неправильне тлумачення людиною або машиною. Що стосується альтернатив, то зазвичай користуються YAML та JSON, з яких YAML націлений на читабельність для людини, а JSON - простіший варіант, дружній до машини. Працюючи з TOML у Swift, ми не маємо нативного парсера. Однак, сторонні бібліотеки на кшталт `TOMLDecoder` полегшують перетворення рядків TOML на типи Swift, зокрема через протоколи `Codable`, введені у Swift 4, які спростили серіалізацію.

## Дивіться також
- Стандарт TOML: https://toml.io
- GitHub для `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Документація Swift по `Codable`: https://developer.apple.com/documentation/swift/codable
- Порівняння форматів серіалізації даних: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
