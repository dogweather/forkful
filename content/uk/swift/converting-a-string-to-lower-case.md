---
title:                "Swift: Перетворення рядка на нижній регістр."
simple_title:         "Перетворення рядка на нижній регістр."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому
Перетворення рядка в нижній регістр є важливою задачею в програмуванні, оскільки воно дозволяє полегшити порівняння та обробку текстової інформації.

## Як це зробити
Існує кілька способів перетворення рядка в нижній регістр у мові Swift. Давайте розглянемо декілька прикладів за допомогою коду та виведемо результат у консолі.

```Swift
let string = "Це Текст в Верхньому Регістрі"
let lowerCaseString = string.lowercased()
print(lowerCaseString) 
// Output: це текст в верхньому регістрі
```

Ви також можете застосувати метод `lowercased()` безпосередньо до будь-якого рядка у вашому коді.

```Swift
let message = "Ласкаво просимо до Swift"
print(message.lowercased())
// Output: ласкаво просимо до swift
```

Для тих, хто працює з мовами, які мають латинський алфавіт, можливо варто використовувати метод `folding` разом із `lowercased()`, щоб обробити рядки з деякими допустимими символами, такими як знаки пунктуації та пробіли.

```Swift
let phrase = "Я вивчаю мову Swift!"
let loweredPhrase = phrase.folding(options: .diacriticInsensitive, locale: nil).lowercased()
print(loweredPhrase)
// Output: я вивчаю мову swift
```

## Глибоке занурення
Коди символів в рядках залежать від конкретної мови, яку ви використовуєте. Це може впливати на результат перетворення в нижній регістр, особливо при використанні методу `lowercased()`. Якщо ви зустрічаєте проблеми з виведенням правильного рядка, можливо варто розглянути використання `folding` разом з `lowercased()`, як описано вище.

Також важливо звернути увагу на різницю між перетворенням в нижній регістр (`lowercased()`) та перетворенням в крапковий регістр (`capitalized()`). Перетворення в нижній регістр робить всі символи в малих літерах, тоді як перетворення в крапковий регістр зробить перший символ великою літерою.

## Дивіться також
- [Документація Apple для методу `lowercased()`](https://developer.apple.com/documentation/swift/string/1786175-lowercased)
- [Стаття про роботу з рядками в Swift](https://swift.org/documentation/#strings)
- [Приклади коду для перетворення рядка в нижній регістр](https://www.hackingwithswift.com/example-code/strings/how-to-make-a-string-lowercased-and-uppercased-string-with-casing)