---
title:    "Swift: Перетворення рядка в нижній регістр"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Чому

Конвертація рядка в нижній регістр є корисним навичкою для програмістів у різних сферах веб-розробки, додатків для мобільних пристроїв та багатьох інших випадків. Це дозволяє ефективно маніпулювати і обробляти дані, що дає змогу покращити якість програми або веб-сайту.

## Як

Конвертування рядка в нижній регістр в Swift дуже просте. Використовуючи вбудовану функцію `.lowercased()`, ми можемо легко змінити всі літери в рядку на нижній регістр. Нижче показана приклад коду та вихідний результат:

```Swift
let text = "Дивовижній Статті"
let lowerCaseText = text.lowercased()

print(lowerCaseText) // виведе: дивовижній статті
```

Також, можна використовувати цю функцію для конкретних символів замість всього рядка. Наприклад:

```Swift
let text = "Дивовижній Статті"
let lowerCaseFirstLetter = text.lowercased().first!

print(lowerCaseFirstLetter) // виведе: д
```

##Глибше

Крім вбудованої функції `.lowercased()`, є інші способи конвертування рядка в нижній регістр в Swift. Наприклад, можна використовувати функцію `folding()`, яка виконує додаткову обробку рядка, наприклад, видаляє діакритичні знаки.

```Swift
let text = "Дивовижній Статті"
let convertedText = text.folding(options: .diacriticInsensitive, locale: .current)

print(convertedText) // виведе: дивовижний статті
```

Також можна використовувати метод `.replacingOccurrences()`, щоб замінити певні символи у рядку, і використати функцію `.uppercased()` для отримання рядка у верхньому регістрі.

## Дивись також

- [Документація Apple про вбудовану функцію `lowercased()`](https://developer.apple.com/documentation/swift/string/1786942-lowercased)
- [Підручник з основ Swift](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html)
- [Туторіал по роботі з рядками в Swift](https://learnappdevelopment.com/swift-strings/)