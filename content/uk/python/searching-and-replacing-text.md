---
title:                "Пошук та заміна тексту"
html_title:           "Python: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Часто при роботі з текстовими файлами або програмуванні потрібно замінювати певні фрагменти тексту на інші. Це може бути завданням для редагування великих документів або автоматизації процесу розробки програмного коду.

## Як

```Python
# Приклад коду для заміни однієї частини тексту на іншу
text = "Цей текст потрібно замінити."
new_text = text.replace("замінити", "відредагувати")
print(new_text)

# Виведе: Цей текст потрібно відредагувати.
```

```Python
# Приклад використання регулярних виразів для заміни тексту
import re

text = "Це речення закінчується точкою."
new_text = re.sub(r"точкою", "крапкою", text)
print(new_text)

# Виведе: Це речення закінчується крапкою.
```

## Глибокий аналіз

Для заміни тексту використовуються різні методи, такі як використання вбудованого методу `replace()` або звернення до регулярних виразів за допомогою модулю `re`. Також варто зазначити, що ці методи можна комбінувати із умовами для більш точної заміни тексту.

## Дивись також

- [Документація Python для методу `replace()`](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Документація Python для модулю `re`](https://docs.python.org/3/library/re.html)
- [Підручник з регулярних виразів для Python](https://www.geeksforgeeks.org/python-regex-cheat-sheet/)