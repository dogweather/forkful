---
title:                "Удаление символов, соответствующих шаблону"
aliases:
- ru/swift/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T23:57:45.953916-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Удаление символов, соответствующих шаблону, означает удаление определенных последовательностей символов из строки на основе заданного шаблона, например, цифр или пунктуации. Программисты делают это для санации ввода, очистки данных или подготовки к обработке, где конкретные шаблоны не нужны.

## Как это сделать:

```swift
import Foundation

// Пример: Удаление всех цифр из строки
let originalString = "Свяжитесь со мной по номеру 123-456-7890 после 09:00 PM."
let digitsPattern = "[0-9]"
let resultString = originalString.replacingOccurrences(of: digitsPattern, with: "", options: .regularExpression)

print(resultString)  // Вывод: "Свяжитесь со мной по номеру -- после : PM."
```

```swift
// Пример: Удаление не-алфавитно-цифровых символов
let messyString = "H3!llo, W%@rld-"
let nonAlphanumericPattern = "[^A-Za-z0-9]"
let cleanString = messyString.replacingOccurrences(of: nonAlphanumericPattern, with: "", options: .regularExpression)

print(cleanString)  // Вывод: "H3lloWrld"
```

## Погружение

До Swift и современного программирования, сопоставление шаблонов было сферой особых средств и языков, таких как `sed`, `awk` или Perl, известных своими возможностями обработки текста. Swift, благодаря своему мощному фреймворку Foundation, упрощает эти задачи в рамках языка, делая их более доступными для разработчиков.

Одна из альтернатив использования регулярных выражений - это итерация по строке с использованием метода `filter` Swift с пользовательским условием, которая также может быть времязатратной и менее читаемой. Регулярные выражения предлагают компактный, хотя иногда и загадочный, способ описания шаблона, который мы хотим удалить или изменить.

Под капотом, когда вы запускаете `replacingOccurrences(of:with:options:)` с опцией `.regularExpression`, Swift использует движок регулярных выражений ICU (International Components for Unicode) для обработки шаблона. ICU - это зрелая, широко используемая библиотека для поддержки Unicode, включая сопоставление шаблонов, которая встроена во многие высокоуровневые языки программирования.

## Смотрите также

- Документация по строкам Swift: https://developer.apple.com/documentation/swift/string
- Регулярные выражения Swift: https://developer.apple.com/documentation/foundation/nsregularexpression
- Руководство пользователя ICU по регулярным выражениям: https://unicode-org.github.io/icu/userguide/strings/regexp.html
