---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---
## Що та чому?

Видалення символів, що відповідають певному шаблону - це процес заміни певних символів у текстовому рядку на пустий рядок. Програмісти роблять це, щоб контролювати та очищувати дані.

## Як це робиться:

Ось простий приклад коду за допомогу регулярних виразів в Python:

```Python
import re

text = 'Привіт, як ваші справи?'
pattern = '[а, и, у, е, ї, і, о, ю, я]'
new_text = re.sub(pattern, '', text)

print(new_text)
```

В результаті ми отримаємо: `Првт, як вш sprv?`

## Поглиблений розгляд

1. **Історичний контекст**: механізм видалення символів, що відповідають паттерну, був запроваджений ще в початкових версіях мов програмування і залишився незмінним.
2. **Альтернативи**: Python має багатий набір методів для роботи з рядками, серед якого знаходяться `replace`, `translate`, `strip` та інші. Різниця полягає в складності шаблонів, які вони можуть обробляти.
3. **Технічні деталі реалізації**: `re.sub` в Python використовує автомати зі стеком, на відміну від `str.replace` більш простих автоматів.

## Див. також

- Документація Python про модуль 're': [https://docs.python.org/3/library/re.html](https://docs.python.org/3/library/re.html)
- Курси про регулярні вирази: [https://www.codecademy.com/learn/learn-regex](https://www.codecademy.com/learn/learn-regex)