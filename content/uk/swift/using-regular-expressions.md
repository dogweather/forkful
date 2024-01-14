---
title:                "Swift: «Використання регулярних виразів»"
simple_title:         "«Використання регулярних виразів»"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

Регулярні вирази - це потужний інструмент для роботи з текстом у Swift. Вони дозволяють шукати, замінювати та перевіряти рядки тексту за допомогою певних шаблонів. Використання регулярних виразів може значно прискорити та полегшити роботу з текстом у ваших програмах.

## Як

Для використання регулярних виразів у Swift потрібно імпортувати бібліотеку `Foundation`.

```Swift 
import Foundation
```

Далі ви можете створити об'єкт типу `NSRegularExpression`, передаючи йому шаблон, за яким буде виконуватись пошук у тексті. Наприклад, якщо ви хочете знайти всі слова, що починаються із літери "a", шаблон може виглядати так:

```Swift 
let pattern = "^a\\w*"
```

Цей шаблон містить основні символи регулярних виразів, такі як `^` для вказування початку рядка та `\w*` для пошуку всіх слів, які починаються з літери "a". Далі ви можете використовувати цей об'єкт `NSRegularExpression` для пошуку в тексті за допомогою методу `firstMatch(in:options:range:)`.

```Swift 
let text = "Apple, apricot, banana, avocado"
if let match = regex.firstMatch(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count)) {
    print(text.substring(with: match.range)) //Apple
}
```

У цьому прикладі ми використовуємо метод `substring(with:)` для отримання знайденого слова з тексту.

## Глибокий занурення

Регулярні вирази у Swift мають своє власне синтаксичне позначення, але вони базуються на синтаксисі регулярних виразів у рядовій мові. Існує багато різних символів та виразів, які можна використовувати у шаблонах, і кожен з них має свої функції та можливості. Щоб більш детально ознайомитися з використанням регулярних виразів у Swift, рекомендуємо прочитати офіційну документацію Apple про NSRegularExpression.

## Дивись також

- [Офіційна документація Apple про NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Стаття про використання регулярних виразів у Swift](https://www.raywenderlich.com/2315-regular-expressions-tutorial-for-swift-part-1-2) від команди Ray Wenderlich
- [Онлайн інструмент для перевірки регулярних виразів](https://regex101.com)