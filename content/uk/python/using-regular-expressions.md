---
title:                "Використання регулярних виразів"
html_title:           "Python: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Чому

З використанням регулярних виразів у програмуванні можна скоротити час і зусилля, потрібні для обробки тексту. Вони дозволяють швидше і ефективніше робити пошук та заміну певних шаблонів у тексті.

## Як

```Python
import re

# Приклад використання регулярних виразів для пошуку email-адрес у тексті
text = "Привіт! Мій email-адреса - example@example.com."
pattern = r"[\w.%+-]+@[\w.-]+\.[a-z]{2,3}"
matches = re.findall(pattern, text)
print(matches)
```
Вивід: ['example@example.com']

## Глибокий пір

Регулярні вирази - це потужний інструмент у рукавичках кожного програміста. Вони дозволяють швидше та більш точно проходити по тексту та витягувати потрібну інформацію. Також, вони допомагають у запобіганні помилок під час обробки великих обсягів даних.

# Дивіться також
- [Офіційна документація Python для модуля re](https://docs.python.org/3/library/re.html)
- [Основи регулярних виразів в Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)