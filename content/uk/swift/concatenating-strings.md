---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?

Стрічкове конкатенацию - це об'єднання двох або більше рядків. Програмісти роблять це для створення динамічного контенту або форматування виводу.

## Як це робиться:

У Swift основний спосіб конкатенувати рядки - використовувати оператора "+".

```Swift
let firstName = "John"
let lastName = "Doe"
let fullName = firstName + " " + lastName

print(fullName) // Outputs: John Doe
```
Але можна також використовувати властивість "appending" або інтерполяцію рядків:

```Swift
let message = "Hello, ".appending(fullName)
print(message) // Outputs: Hello, John Doe
```
```Swift
let intro = "My name is \(fullName)"
print(intro) // Outputs: My name is John Doe
```
## Поглиблена інформація

1. **Історичний контекст**: Основа конкатенування рядків бере витоки з перших мов програмування, коли було потрібно об'єднувати символи за допомогою кодів ASCII.

2. **Альтернативи**: Окрім конкатенування, в Swift можна використовувати форматовані рядки або багаторядкові рядки.

```Swift
let formattedString = String(format: "Hello, %@. I am %@", arguments: [lastName, firstName])
```
```Swift
let multilineString = """
  This is line 1.
  This is line 2.
  This is line \(3).
  """ 
```
3. **Деталі реалізації**: Коли ви конкатенуєте рядки, Swift створює новий об'єкт рядка. Це важливо, якщо ви збираєтесь об'єднати декілька рядків в циклі, оскільки це може вплинути на продуктивність вашого коду.

## Дивіться також

1. [Документація Swift: Strings and Characters](https://developer.apple.com/documentation/swift/string)
2. [Stack Overflow: String concatenation vs. interpolation in Swift](https://stackoverflow.com/questions/24092884/get-nth-character-of-a-string-in-swift-programming-language)