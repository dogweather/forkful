---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:45.953916-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043E Swift \u0438 \u0441\u043E\u0432\u0440\u0435\u043C\u0435\
  \u043D\u043D\u043E\u0433\u043E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0440\u043E\u0432\u0430\u043D\u0438\u044F, \u0441\u043E\u043F\u043E\u0441\u0442\
  \u0430\u0432\u043B\u0435\u043D\u0438\u0435 \u0448\u0430\u0431\u043B\u043E\u043D\u043E\
  \u0432 \u0431\u044B\u043B\u043E \u0441\u0444\u0435\u0440\u043E\u0439 \u043E\u0441\
  \u043E\u0431\u044B\u0445 \u0441\u0440\u0435\u0434\u0441\u0442\u0432 \u0438 \u044F\
  \u0437\u044B\u043A\u043E\u0432, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A\
  \ `sed`, `awk` \u0438\u043B\u0438 Perl,\u2026"
lastmod: '2024-04-05T21:53:46.032303-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043E Swift \u0438 \u0441\u043E\u0432\u0440\u0435\u043C\u0435\u043D\
  \u043D\u043E\u0433\u043E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u044F, \u0441\u043E\u043F\u043E\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u0438\u0435 \u0448\u0430\u0431\u043B\u043E\u043D\u043E\u0432\
  \ \u0431\u044B\u043B\u043E \u0441\u0444\u0435\u0440\u043E\u0439 \u043E\u0441\u043E\
  \u0431\u044B\u0445 \u0441\u0440\u0435\u0434\u0441\u0442\u0432 \u0438 \u044F\u0437\
  \u044B\u043A\u043E\u0432, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `sed`,\
  \ `awk` \u0438\u043B\u0438 Perl, \u0438\u0437\u0432\u0435\u0441\u0442\u043D\u044B\
  \u0445 \u0441\u0432\u043E\u0438\u043C\u0438 \u0432\u043E\u0437\u043C\u043E\u0436\
  \u043D\u043E\u0441\u0442\u044F\u043C\u0438 \u043E\u0431\u0440\u0430\u0431\u043E\u0442\
  \u043A\u0438 \u0442\u0435\u043A\u0441\u0442\u0430."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\u043E\
  \u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\u0432\
  \u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\u043B\u043E\u043D\u0443"
weight: 5
---

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
