---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що та навіщо?
Щодо видалення символів, які відповідають шаблону - це процес фільтрації рядка, в умовах якого ми видаляємо певні символи на основі заданих критеріїв. Програмісти роблять це для очищення, модифікації та керування даними - важливі аспекти багатьох програмних задач.

## Як це зробити:
За допомогою Swift, ми можемо легко видалити певний набір символів із рядка за допомогою методу `filter()`. Ось простий приклад:

``` Swift
let oldString = "Hello, World!"
let charactersToRemove: [Character] = [",", "!", " "]
let newString = oldString.filter { !charactersToRemove.contains($0) }
print(newString)
```
Виходить:

``` Swift
"HelloWorld"
```
У цьому сценарію ми видаляємо коми, знак оклику та пробіли.

## Поглиблений занурення
Історично, можливість видалити символи з рядка, які відповідають певному шаблону, була важливою частиною багатьох мов програмування, але Swift робить це набагато простіше з допомогою методу `filter()`. Альтернативно, ви могли б використовувати регулярні вирази для більш складних шаблонів, але для простих випадків це зазвичай не потрібно. Щодо деталей впровадження, Swift `filter()` працює шляхом ітерації кожного символу в рядку та перевірки, чи відповідає він зазначеному критерію.

## Дивіться також
1. Apple Swift Documentation: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
2. Swift String and Characters: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
3. Swift Standard Library: [https://developer.apple.com/documentation/swift/swift_standard_library](https://developer.apple.com/documentation/swift/swift_standard_library)