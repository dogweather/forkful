---
title:                "Использование регулярных выражений"
aliases: - /ru/swift/using-regular-expressions.md
date:                  2024-01-29T00:03:42.158061-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Регулярные выражения, или regex, — это шаблоны, используемые для поиска комбинаций символов в строках. Программисты используют их для поиска, редактирования или проверки текста, что делает задачи, связанные с манипуляциями со строками, более эффективными и с меньшей вероятностью ошибок.

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
