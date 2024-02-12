---
title:                "Видалення символів за візерунком"
aliases:
- /uk/swift/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:43:47.229610-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і Чому?
У Swift, видалення символів за шаблоном – це процес прибирання зі стрічки окремих букв або груп символів, які відповідають певному критерію. Програмісти це роблять для очищення даних, оптимізації інформації, чи подготовки тексту до обробки.

## Як це зробити:
В Swift, щоб видалити символи за шаблоном, використовуйте `NSRegularExpression` і метод `stringByReplacingMatches`. Ось простий приклад:

```Swift
import Foundation

func deleteCharacters(matching pattern: String, from input: String) -> String {
    let regex = try! NSRegularExpression(pattern: pattern)
    let range = NSRange(input.startIndex..<input.endIndex, in: input)
    return regex.stringByReplacingMatches(in: input, options: [], range: range, withTemplate: "")
}

let originalString = "Hello, World! 123."
let pattern = "[^A-Za-z ]" // Видалення усіх символів, крім літер алфавіту і пробілів

let cleanedString = deleteCharacters(matching: pattern, from: originalString)
print(cleanedString) // Виведе: "Hello World"
```

Sample output:
```
Hello World
```

## Детальніше:
Історично, регулярні вирази, як і `NSRegularExpression`, використовуються для таких операцій з текстами. Альтернатива – `String` методи в Swift, але регулярні вирази кращі для складних шаблонів. Важливо контролювати виключення `try!` при створенні `NSRegularExpression` – помилки можуть з'явитись, якщо шаблон неправильний.

## Також подивіться:
Для більш детального розуміння регулярних виразів:
- [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)

Ознайомтеся з документацією Swift щодо рядків і символів:
- [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

Поглибити знання з роботою з текстовими даними в Swift допоможе:
