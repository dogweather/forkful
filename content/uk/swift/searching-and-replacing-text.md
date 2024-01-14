---
title:                "Swift: Пошук і заміна тексту"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Можливо, у вас є великий проект з багатьма рядками коду, і ви хочете замінити певне розміщення тексту на інше. Заміна тексту може зекономити ваш час і забезпечити правильне функціонування вашого програмного забезпечення.

## Як це зробити

У Swift є кілька способів заміни тексту. Один з них - використання методу ```replacingOccurrences``` для заміни всіх входжень даного рядка на інший рядок. Наприклад:

```
let originalString = "Привіт, Ukraine!"
let newString = originalString.replacingOccurrences(of: "Ukraine", with: "мій друг")
print(newString)
// виводить "Привіт, мій друг!"
```

Інший спосіб - використання регулярних виразів з методом ```replacingMatches``` для заміни специфічних входжень тексту. Наприклад, якщо ви хочете замінити всі літери "а" на "е", ви можете використати наступний код:

```
let originalString = "apple"
let newString = originalString.replacingOccurrences(of: "[аA]", with: "е", options: .regularExpression)
print(newString)
// виводить "eрple"
```

## Розглиблення

Заміна тексту може бути більш складним процесом, коли ви працюєте з більшими обсягами коду. Є кілька корисних методів, які можуть полегшити цю задачу, таких як ```trimmingCharacters``` для видалення пропусків, ```range(of:)``` для пошуку інтервалів тексту та ```replacingCharacters``` для заміни символів у певному інтервалі. Важливо також враховувати особливості релізного і девелоперського режимів, щоб не зробити помилки в заміні тексту на живому проекті.

## Дивись також

Для додаткової інформації про заміну тексту, рекомендуємо переглянути наступні посилання:

- [Документація Swift про метод заміни тексту](https://developer.apple.com/documentation/foundation/nsstring/1413216-replacingoccurrences)
- [Курс з основ програмування на Swift на Coursera](https://www.coursera.org/learn/swift-programming-syntax)
- [Стаття на тему заміни тексту в Swift на блозі Ray Wenderlich](https://www.raywenderlich.com/170-codable-tutorial-getting-started-with-codable)