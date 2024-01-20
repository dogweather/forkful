---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що та чому?

"Конкатенувати" значає просто об'єднати дві або більше рядків в одне ціле. Програмісти роблять це, щоб маніпулювати даними, форматувати вивід, шаблони тексту та генерувати код.

## Як?

Ось основний спосіб, яким ви можете злити рядки в Python:

```Python 
str1 = "Привіт, "
str2 = "світе!"
print(str1 + str2)
```
Вивід:
```Python 
"Привіт, світе!"
```

## Докладніше

Конкатенація рядків належить до основних концепцій в програмуванні, ще з часів мови C.
Альтернативи? Є! Їх не так багато, але вони можуть стати в нагоді. Функція `join()` і інтерполяція рядків, або `f-strings`,  в Python:

```Python 
name = "Світ"
print(f"Привіт, {name}!")
```

Вивід:
```Python 
"Привіт, Світ!"
```

Щодо виконання, Python створює новий об'єкт, коли ви об'єднуєте рядки. Оскільки рядки в Python є не змінюваними, він не може просто додавати до існуючого рядка.

## Додаткові ресурси

- Python Documentation: [Вбудовані типи](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Python For Beginners: [Рядки](https://www.pythonforbeginners.com/basics/string-manipulation-in-python)
- Python Tutor: [Concise Introduction to Strings](https://pythontutor-ru.herokuapp.com/course18/v2/ch02.html)