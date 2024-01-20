---
title:                "Робота з yaml"
html_title:           "Haskell: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

Що і чому?
YAML (YAML Ain't Markup Language) - це формат даних, призначений для збереження структурованої інформації у зручному форматі. Це звичайний текстовий файл, який є більш зрозумілим для людей, ніж машинна мова, тому програмісти використовують YAML для зручності та читабельності.

Як це зробити?
Кодування з YAML дуже просте за допомогою мови програмування Haskell. Для початку, ми повинні імпортувати модуль Data.YAML та вказати, який саме файлик ми хочемо зчитати. Потім, ми можемо використовувати вбудовані функції для обробки даних у форматі YAML. Ось приклад коду та результату виконання:

```Haskell
import Data.YAML
myYAML <- decodeFile "example.yaml"
print myYAML
```

Результат:

```
Just (Object (fromList [("name",String "John"),("age",Number 25),("hobbies",Seq [String "coding",String "hiking",String "cooking"])]))
```

Глибокий пір?
Дата випуску YAML припадає на 2001 рік, і її створила команда програмістів для полегшення обміну даними у форматі, зрозумілим для людей. Альтернативою YAML є JSON (JavaScript Object Notation), який також має читабельний синтаксис, але є більш популярним у веб-розробці. Реалізація обробки YAML в Haskell базується на стандартних типах даних, тому навчання її використовувати буде корисною навичкою для роботи з іншими форматами даних.

Дивись також:
- [YAML офіційний сайт](https://yaml.org/)
- [JSON vs YAML: яка різниця?](https://www.educative.io/edpresso/json-vs-yaml-whats-the-difference)