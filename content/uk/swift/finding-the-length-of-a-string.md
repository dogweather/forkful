---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

**## Що і для чого?**

Знаходження довжини рядка - це визначення кількості символів, які включені до рядка. Програмісти роблять це, оскільки це часто потрібно для виконання циклів, порівняння рядків та вирішення інших типових завдань.

**## Як зробити:**

Код та приклади виведення в кодовому блоку Swift:

```Swift
let helloWorld = "Привіт, Світ"
let lengthOfString = helloWorld.count
print(lengthOfString)
```

У вищенаведеному коді, ми визначаємо довжину рядка через власність `.count` рядка, і потім друкуємо цю довжину. Виведення буде `12`.

**## Поглиблений занурення:**

**Історичний контекст:** В ранніх версіях Swift, замість `.count` використовувався метод `.characters.count`. Але від Swift 4, кількість символів рядка можна отримати прямо через властивість `.count`.

**Альтернативи:** Можна використовувати `.lengthOfBytes(using:)` для отримання довжини рядка в байтах, але це не те ж саме, що і кількість символів.

**Деталі реалізації:** Пам'ятайте, властивість `.count` строк в Swift повертає кількість символів Unicode. Тому якщо в рядку є символи, які виражаються через декілька знакових єдиниц (наприклад, емодзі), `.count` може повернути більше значення, ніж ви очікуєте.

**## Дивіться також:**

[Стрічки та символи](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

[Кодування Unicode](https://unicode.org/faq/utf_bom.html#utf8-4)

[Swift рядок](https://developer.apple.com/documentation/swift/string)