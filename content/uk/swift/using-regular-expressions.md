---
title:                "Swift: Використання регулярних виразів"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Тому, чому варто користуватися регулярними виразами?

Регулярні вирази є потужним інструментом для пошуку та заміни шаблонів у тексті програми. Вони дозволяють ефективно виконувати різноманітні завдання, що пов'язані з обробкою рядків у Swift.

## Як користуватися регулярними виразами

```Swift
let input = "Привіт, я програмую на Swift!"
let pattern = "Swift"
let regex = try! NSRegularExpression(pattern: pattern)
if let match = regex.firstMatch(in: input, range: NSRange(input.startIndex..., in: input)) {
    print("Знайдено збіг з шаблоном: \(input[Range(match.range, in: input)!])")
}
```

У цьому прикладі ми перевіряємо, чи містить рядок "Привіт, я програмую на Swift!" слово "Swift". Для цього ми створюємо регулярний вираз за допомогою класу NSRegularExpression та шукаємо перший збіг за допомогою методу firstMatch(in:range:). Якщо збіг знайдено, виводимо знайдений текст.

## Глибоке дослідження регулярних виразів

Регулярні вирази використовуються не тільки для простого пошуку тексту, але і для більш складних операцій, таких як заміна певних шаблонів, отримання підмасивів тощо. Також, цей інструмент дозволяє використовувати різні опції та модифікатори для більш гнучкого пошуку. Щоб дізнатися більше про регулярні вирази в Swift, рекомендуємо ознайомитися з наступними посиланнями:

## Дивіться також

- [Документація Swift про регулярні вирази](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Стаття про використання регулярних виразів в Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)