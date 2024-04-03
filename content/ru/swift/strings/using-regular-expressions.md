---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:42.158061-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0412 Swift \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\
  \u044B \u0441 regex \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \u0441\u044F \u043A\u043B\u0430\u0441\u0441 `NSRegularExpression`. \u0412\u044B\
  \ \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u044F\u0435\u0442\u0435 \u0448\u0430\
  \u0431\u043B\u043E\u043D, \u0441\u043E\u0437\u0434\u0430\u0435\u0442\u0435 \u043E\
  \u0431\u044A\u0435\u043A\u0442 regex, \u0430 \u0437\u0430\u0442\u0435\u043C \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0435\u2026"
lastmod: '2024-03-13T22:44:45.665508-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Swift \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441\
  \ regex \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0441\u044F\
  \ \u043A\u043B\u0430\u0441\u0441 `NSRegularExpression`."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Как использовать:
В Swift для работы с regex используется класс `NSRegularExpression`. Вы определяете шаблон, создаете объект regex, а затем используете его для поиска или замены текста. Вот базовый пример:

```Swift
import Foundation

let input = "Позвони мне по номеру 555-1234 или 555-5678."
let pattern = "\\d{3}-\\d{4}" // Соответствует шаблону типа 555-1234

do {
    let regex = try NSRegularExpression(pattern: pattern)
    let matches = regex.matches(in: input, range: NSRange(input.startIndex..., in: input))
    
    for match in matches {
        if let range = Range(match.range, in: input) {
            let phoneNumber = String(input[range])
            print("Найденный номер телефона: \(phoneNumber)")
        }
    }
} catch {
    print("Ошибка regex: \(error.localizedDescription)")
}
```

Пример вывода:
```
Найденный номер телефона: 555-1234
Найденный номер телефона: 555-5678
```

## Погружение в тему
Регулярные выражения существуют с 1950-х годов, возникнув в теории формальных языков и получив широкое распространение в инструментах Unix. В Swift мы используем класс `NSRegularExpression`, унаследованный от Objective-C, который опирается на библиотеку ICU для поддержки Unicode.

Альтернативами regex в Swift являются использование методов `contains`, `split` или `range(of:)` класса `String` для простых случаев. Для более сложного поиска шаблонов Swift не предлагает встроенных альтернатив regex.

При реализации regex крайне важно оптимизировать шаблон для избежания медленного поиска, особенно при работе с большими объемами текста. Кроме того, помните, что операции regex могут генерировать исключения, поэтому всегда обрабатывайте их с помощью блоков `try-catch`.

## Смотрите также
- [Документация NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Документация Swift String](https://developer.apple.com/documentation/swift/string)
- [Руководство Рэя Вендерлиха по использованию NSRegularExpression в Swift](https://www.raywenderlich.com/2725-nsregularexpression-tutorial-and-cheat-sheet)
