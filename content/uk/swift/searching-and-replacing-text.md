---
title:                "Пошук та заміна тексту"
html_title:           "Swift: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Що і чому?

Заміна тексту - це процес пошуку та заміни певного тексту в програмі. Програмісти зазвичай виконують це для зміни, оновлення або покращення коду.

Як це зробити:

```Swift
let originalString = "Привіт, друзі!"
let replaced = originalString.replacingOccurrences(of: "Привіт", with: "Привітсуки")
print(replaced)

Вивід: "Привітсуки, друзі!"
```

Глибші деталі:

Історичний контекст: Заміна тексту була дуже важливою функцією у багатьох програмних мовах, оскільки дає можливість змінювати значення змінних в коді.

Альтернативи: Також можна здійснити заміну тексту за допомогою функцій, які вже вбудовані в деякі масиви та об'єкти.

Деталі реалізації: У Swift існує функція `replacingOccurrences` для роботи зі зміною тексту. Вона приймає два параметри - оригінальний текст та той, на який треба замінити - і повертає змінений результат.

Дивись також:

За додатковою інформацією щодо заміни тексту у Swift можна звернутися до [документації Apple](https://developer.apple.com/documentation/foundation/nsstring/1411854-replacingoccurrences) або подивитися [відеоурок](https://www.youtube.com/watch?v=pKRvGyXS4Lo) на YouTube. Крім того, можливо зацікавлять [підручник з основ Swift](https://swiftbook.ru/content/languageguide/collection-types-strings-and-characters/).