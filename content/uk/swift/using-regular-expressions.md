---
title:                "Використання регулярних виразів"
html_title:           "Swift: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що і чому? 
Використання регулярних виразів (Regular Expressions) дозволяє програмістам швидко та ефективно виконувати пошук та заміну тексту, використовуючи певні шаблони. Це особливо корисно при обробці текстових даних, таких як імена, адреси електронної пошти, номери телефонів та інше. Програмісти використовують регулярні вирази, щоб спростити складні пошукові запити та автоматизувати рутинні завдання.

## Як: 
```Swift
let string = "Привіт! Мене звати Лілія, а мій електронний лист - liliya@email.com"
let regex = try NSRegularExpression(pattern: "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,64}", options: .caseInsensitive)
let matches = regex.matches(in: string, options: [], range: NSRange(location: 0, length: string.utf16.count))
for match in matches {
  let email = (string as NSString).substring(with: match.range)
  print(email)
}
```

Вищенаведений приклад дозволяє знайти та вивести адреси електронної пошти з рядка тексту. Використовуючи певний шаблон, ми можемо легко визначити, що це є адреса електронної пошти та отримати її значення. Цей підхід дозволяє ефективно обробляти великі обсяги даних та дозволяє точно визначати необхідну інформацію.

## Глибші дослідження: 
Історично, регулярні вирази виникли у 1956 році, коли математик Стівен Коул (Stephen Cole Kleene) представив математичний формалізм для описує регулярні мови. Згодом, в 1968 році, Кен Томпсон (Ken Thompson) використав ці ідеї для створення першого пошукового двигуна під назвою "QED". У сучасному програмуванні, наряду з регулярними виразами, існує багато альтернативних засобів обробки тексту, таких як функції додатків рядків або регулярні вирази на базі дерев або негативних наборів.

## Дивіться також: 
- [Офіційна документація Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID287)
- [Регулярні вирази на сайті RegExr](https://regexr.com/)
- [Відеоуроки по регулярним виразам на YouTube](https://www.youtube.com/playlist?list=PL1QoYy59E_fiedCVpgabxVh4BoANL8gqn)