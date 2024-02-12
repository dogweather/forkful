---
title:                "Преобразование строки в верхний регистр"
aliases:
- /ru/swift/capitalizing-a-string.md
date:                  2024-01-28T23:55:44.740913-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Капитализация означает изменение первой буквы слов на прописные; в строках, это часто касается форматирования или создания пользовательского интерфейса. Программисты используют капитализацию строк для улучшения читаемости, соблюдения грамматических правил или соответствия руководству по стилю.

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
