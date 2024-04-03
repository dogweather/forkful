---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:44.740913-07:00
description: "\u041A\u0430\u043F\u0438\u0442\u0430\u043B\u0438\u0437\u0430\u0446\u0438\
  \u044F \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0438\u0437\u043C\u0435\
  \u043D\u0435\u043D\u0438\u0435 \u043F\u0435\u0440\u0432\u043E\u0439 \u0431\u0443\
  \u043A\u0432\u044B \u0441\u043B\u043E\u0432 \u043D\u0430 \u043F\u0440\u043E\u043F\
  \u0438\u0441\u043D\u044B\u0435; \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\u0445\
  , \u044D\u0442\u043E \u0447\u0430\u0441\u0442\u043E \u043A\u0430\u0441\u0430\u0435\
  \u0442\u0441\u044F \u0444\u043E\u0440\u043C\u0430\u0442\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u044F \u0438\u043B\u0438 \u0441\u043E\u0437\u0434\u0430\u043D\u0438\
  \u044F \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044C\u0441\
  \u043A\u043E\u0433\u043E \u0438\u043D\u0442\u0435\u0440\u0444\u0435\u0439\u0441\u0430\
  .\u2026"
lastmod: '2024-03-13T22:44:45.652972-06:00'
model: gpt-4-0125-preview
summary: "\u041A\u0430\u043F\u0438\u0442\u0430\u043B\u0438\u0437\u0430\u0446\u0438\
  \u044F \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0438\u0437\u043C\u0435\
  \u043D\u0435\u043D\u0438\u0435 \u043F\u0435\u0440\u0432\u043E\u0439 \u0431\u0443\
  \u043A\u0432\u044B \u0441\u043B\u043E\u0432 \u043D\u0430 \u043F\u0440\u043E\u043F\
  \u0438\u0441\u043D\u044B\u0435; \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\u0445\
  , \u044D\u0442\u043E \u0447\u0430\u0441\u0442\u043E \u043A\u0430\u0441\u0430\u0435\
  \u0442\u0441\u044F \u0444\u043E\u0440\u043C\u0430\u0442\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u044F \u0438\u043B\u0438 \u0441\u043E\u0437\u0434\u0430\u043D\u0438\
  \u044F \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044C\u0441\
  \u043A\u043E\u0433\u043E \u0438\u043D\u0442\u0435\u0440\u0444\u0435\u0439\u0441\u0430\
  ."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 2
---

## Как:
Swift делает капитализацию строк простой. Вот быстрый обзор:

```Swift
let lowercasedString = "hello, world!"
let titleCased = lowercasedString.capitalized // "Hello, World!"
let uppercasedString = lowercasedString.uppercased() // "HELLO, WORLD!"

// Пример вывода:
print(titleCased)  // Выводит "Hello, World!"
print(uppercasedString)  // Выводит "HELLO, WORLD!"
```

Для большего контроля познакомимся с `Locale`:

```Swift
let sentence = "the quick brown fox"
let titleCasedWithLocale = sentence.capitalized(with: Locale(identifier: "en_US"))
// "The Quick Brown Fox"

// Пример вывода:
print(titleCasedWithLocale)  // Выводит "The Quick Brown Fox"
```

## Глубокое погружение
Капитализация в программировании существует столько же, сколько и обработка цифрового текста - это всё о соответствии ожиданиям пользователя. Хотя `capitalized` в Swift стандартизирует строки в Заглавный Регистр, где первый символ каждого слова прописной, есть нюансы.

Исторически, программистам нужны были собственные методы для капитализации, учитывая особые случаи самостоятельно. `Capitalized` в Swift учитывает локаль, что важно для собственных имён или правил регистра, специфичных для локали.

Говоря о альтернативах, те, кто не удовлетворён `capitalized`, часто обращаются к regex или пишут расширения к `String` для более сложных правил. С точки зрения реализации, `capitalized` по сути является встроенным методом, который проходит через строку, применяя прописные буквы к первому символу после символа, не являющегося буквой.

```Swift
extension String {
    func customCapitalized() -> String {
        return self.lowercased().replacingOccurrences(of: "\\b\\w", with: { 
            guard let firstChar = $0.first else { return $0 }
            return String(firstChar).uppercased() + $0.dropFirst()
        }, options: .regularExpression)
    }
}
```

Вышеупомянутое расширение использует регулярное выражение для капитализации первой буквы каждого слова.

## Смотрите также
Для более глубокого изучения манипуляций со строками в Swift, вот некоторые полезные ресурсы:
- [Документация Swift по Строкам](https://developer.apple.com/documentation/swift/string)
- [Учебник по Строкам в Swift от Ray Wenderlich](https://www.raywenderlich.com/5492-working-with-strings-in-swift)
