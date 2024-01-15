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

## Чому
Регулярні вирази - це потужний інструмент для роботи з рядками тексту в Swift. Вони можуть бути використані для пошуку, заміни, або валідації певного тексту, що робить роботу з текстом більш ефективною та зручною.

## Як
Найбільш простий спосіб використовувати регулярні вирази в Swift - це за допомогою вбудованого класу ```NSRegularExpression```. Давайте розглянемо приклад використання регулярного виразу для пошуку у рядку тексту:
```Swift
// Створимо регулярний вираз для пошуку слова "код"
let regex = try! NSRegularExpression(pattern: "код")
// Рядок, в якому будемо шукати
let string = "Я люблю писати код, це моє хобі!"
// Використовуємо метод "firstMatch()" для пошуку першого входження слова "код" у рядку та отримання інформації про його розташування
if let match = regex.firstMatch(in: string, options: [], range: NSRange(location: 0, length: string.utf16.count)) {
    // Виводимо результат у консоль
    print("Співпадіння знайдено у рядку зі стартовою позицією \(match.range.location) та довжиною \(match.range.length)")
    // Виводимо знайдений текст
    print("Знайдено текст: \(string[Range(match.range, in: string)!])")
}
// Результат виконання програми: Співпадіння знайдено у рядку зі стартовою позицією 19 та довжиною 3
// Знайдено текст: код
```
Також можна використовувати регулярні вирази для заміни тексту, наприклад:
```Swift
// Замінимо слово "хобі" на "заняття"
let updatedString = regex.replacingMatches(in: string, options: [], range: NSRange(location: 0, length: string.utf16.count), withTemplate: "заняття")
// Результат: "Я люблю писати код, це моє заняття!"
```

## Deep Dive
Щоб краще зрозуміти сутність регулярних виразів, важливо ознайомитись з основними метасимволами та методами класу ```NSRegularExpression```. Детальніше про це можна прочитати в документації Apple для регулярних виразів у Swift. Також, для покращення навичок використання регулярних виразів, рекомендується використовувати онлайн-інструменти для перевірки та експериментування з регулярними виразами, наприклад, Regex101 чи RegExr.

## Дивіться також
- [Документація Apple для регулярних виразів у Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex101](https://regex101.com/)
- [RegExr](https://regexr.com/)