---
title:                "Приголосний рядок"
html_title:           "Python: Приголосний рядок"
simple_title:         "Приголосний рядок"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це & навіщо? 

Створення великої літери у рядку – це перетворення першої букви рядка в верхній регістр. Програмісти використовують це, наприклад, для нормалізації даних під час обробки тексту.

## Як виконати:

Просто використовуйте рядковий метод `.capitalize()`. Ось два приклади:

```Python 
s = "hello, python world!"
capitalized_s = s.capitalize()
print(capitalized_s)
```

Це дасть такий результат:
```Python
"Hello, python world!"
```
Цей метод робить лише першу букву великою, решту букв в рядку переводить у нижній регістр.

```Python 
s = "HELLO, PYTHON WORLD!"
capitalized_s = s.capitalize()
print(capitalized_s)
```

Це дасть такий результат:
```Python
"Hello, python world!"
```

## Поглиблено:

Історично, велику букву використовували на початку речень або для імені особи. В Python метод `capitalize()` був включений у початкову версію (Python 1.0). Як альтернатива, ви можете використовувати функцію `title()`, яка робить великою першу букву кожного слова. Метод `capitalize()` працює шляхом перетворення рядка в Unicode, змінення першого символу і повернення його назад у рядок.

```Python 
s = "hello, python world!"
title_s = s.title()
print(title_s)
```

Це дасть такий результат:
```Python
"Hello, Python World!"
```

## Дивіться також:

1. [Python Strings](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str) (документація про рядки Python)
3. [Difference between str.title() and str.capitalize()](https://stackoverflow.com/questions/23877049/difference-between-str-title-and-str-capitalize) (обговорення на StackOverflow)