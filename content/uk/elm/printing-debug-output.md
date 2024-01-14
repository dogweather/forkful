---
title:    "Elm: Вивід відлагодження на друк"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Чому

Настільки важливо виводити відладку?

Доведення проблем в програмі може бути дуже складним завданням, і саме тому виведення відладкового результату є дуже цінним інструментом для розробників. Це дозволяє нам побачити різні значення змінних та станів програми в режимі відладки, що допомагає нам знайти та виправити помилки.

## Як це зробити

```elm
fruit : String
fruit = "apple"

debug fruit -- виводить "apple" в консоль відладки
```

Приклад коду дозволяє нам використовувати вбудовану функцію `debug` для виведення відладкової інформації. Ми можемо передавати будь-які значення у цю функцію, і вона автоматично відобразить їх у консолі відладки. Це дуже корисно для перевірки значень змінних та уникнення помилок.

## Глибше

Ви можете використовувати більш складні вирази для виведення відладкових повідомлень, наприклад, додавання іншої строки до даних чи використання умовних виразів. Також, при роботі зі складними структурами даних, ви можете використовувати функцію `Debug.toString`, яка перетворить будь-який тип даних у рядок для виведення в консоль відладки.

## Дивіться також

- [Документація Elm про вивід відладки](https://guide.elm-lang.org/debugging/debugging.html)
- [Відео-урок про використання відладки у Elm](https://www.youtube.com/watch?v=3zD1xqN0zZE)
- [Стаття про використання відладки в промислових Elm проектах](https://medium.com/@dan_abramov/debugging-node-js-in-production-9620a44942e8)