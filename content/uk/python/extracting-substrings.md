---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і чому?
Вибір підрядків (substring) - це процес вилучення певних частин рядків. Програмісти роблять це, щоб працювати з конкретними даними чи властивостями рядків.

## Як це робити:
Python надає декілька шляхів для вилучення підрядків. Давайте дізнаємось як.

```Python
# Використовуємо індексацію
string = "Привіт, Python!"
print(string[7:13])  # Виведе "Python"
```

```Python
# Використовуємо функцію slice()
string = "Привіт, Python!"
slice_obj = slice(7, 13)
print(string[slice_obj])  # Теж виведе "Python"
```

## Поглиблений розбір 
Вилучення підрядків виникло вже на самому початку історії програмування і було необхідним для обробки текстових даних. Python надає гнучкий та зручний підхід до вилучення підрядків.

Альтернативою могла б бути ручна обробка рядків, але це займає більше часу і примусить вас писати більше коду.

Що стосується внутрішньої реалізації, Python використовує об’єкти "slice" для вилучення підрядків. Коли ви використовуєте синтаксис з квадратними дужками (наприклад, ```string[7:13]```), Python автоматично створює об'єкт "slice" і використовує його для вилучення підрядку.

## Дивись також
1. Детальний підручник по Python рядках : https://realpython.com/python-strings/
2. Python slice(): https://www.programiz.com/python-programming/methods/built-in/slice
3. Проект Python: Вбудовані функції: https://docs.python.org/3/library/functions.html#slice