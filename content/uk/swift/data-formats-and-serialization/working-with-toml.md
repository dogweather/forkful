---
aliases:
- /uk/swift/working-with-toml/
date: 2024-01-26 04:27:16.138259-07:00
description: "TOML (Tom's Obvious, Minimal Language - \u041E\u0447\u0435\u0432\u0438\
  \u0434\u043D\u0430 \u0442\u0430 \u043C\u0456\u043D\u0456\u043C\u0430\u043B\u0456\
  \u0441\u0442\u0438\u0447\u043D\u0430 \u043C\u043E\u0432\u0430 \u0422\u043E\u043C\
  \u0430) - \u0446\u0435 \u0444\u043E\u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0456\
  \u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445\
  , \u044F\u043A\u0438\u0439 \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\
  \u0442\u0438 \u0437\u0430\u0432\u0434\u044F\u043A\u0438 \u0441\u0432\u043E\u0457\
  \u0439 \u0447\u0456\u0442\u043A\u0456\u0439\u2026"
lastmod: 2024-02-18 23:09:01.002119
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language - \u041E\u0447\u0435\u0432\u0438\u0434\
  \u043D\u0430 \u0442\u0430 \u043C\u0456\u043D\u0456\u043C\u0430\u043B\u0456\u0441\
  \u0442\u0438\u0447\u043D\u0430 \u043C\u043E\u0432\u0430 \u0422\u043E\u043C\u0430\
  ) - \u0446\u0435 \u0444\u043E\u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0456\u0430\
  \u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\
  \u043A\u0438\u0439 \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0442\
  \u0438 \u0437\u0430\u0432\u0434\u044F\u043A\u0438 \u0441\u0432\u043E\u0457\u0439\
  \ \u0447\u0456\u0442\u043A\u0456\u0439\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
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
