---
title:                "Swift: Видалення символів, які відповідають вказаному шаблону"
simple_title:         "Видалення символів, які відповідають вказаному шаблону"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Набуваючи знання в програмуванні Swift, ви можете зіткнутися з задачею видалення символів, які відповідають певному шаблону. Це може бути корисно для зроблення певних маніпуляцій зі строками або для фільтрації вхідних даних.

## Як це зробити

Цей процес можна виконати за допомогою методу `replacingOccurrences(of:, with:)` у кожному екземплярі `String`. Першим параметром буде шаблон, який потрібно знайти, а другий - той, який потрібно замінити. Наприклад, якщо ми хочемо видалити всі числа зі строкі "Hello123World", можна використати наступний код:

```Swift
let stringWithNumbers = "Hello123World"
let replacedString = stringWithNumbers.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)
print(replacedString) // Output: HelloWorld
```

У цьому прикладі, "[0-9]" - це шаблон, який відповідає будь-якому числу. Ви можете змінити цей шаблон, щоб видаляти символи, які відповідають конкретним потребам вашого проекту.

## Поглиблене вивчення

Крім методу `replacingOccurrences(of:, with:)`, у Свіфт також є інші засоби для видалення символів, які відповідають певному шаблону. Наприклад, ви можете використовувати метод `trimmingCharacters(in:)`, щоб видалити всі символи з країв стрічки, які відповідають певному набору символів. Також, для заміни всіх символів, які відповідають шаблону, вам може стати в пригоді метод `replacingOccurrences(of:, with:, options: .regularExpression)`.

## Дивись також

- [Документація Apple про `String.replacingOccurrences(of:, with:, options:)`](https://developer.apple.com/documentation/foundation/nsstring/1410783-replacingoccurrences)
- [Туторіал про регулярні вирази у Свіфт](https://www.raywenderlich.com/158831/regular-expressions-swift)
- [Генератор регулярних виразів для Свіфт](https://regexr.com/swift)