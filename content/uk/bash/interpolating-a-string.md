---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Інтерполяція рядків - це процес вставки змінних або виразів прямо в рядок. Програмісти роблять це для більшого контролю над форматуванням і представленням результатів.

## Як це зробити:

В Bash інтерполяцію рядків можна реалізувати за допомогою подвійних лапок. Спробуйте наступний код:

```bash
name="Олег"
echo "Привіт, $name"
```

Результат:

```bash
Привіт, Олег
```

## Занурення в деталі:

Історично для інтерполяції рядків використовували багато різних способів. Bash вважається одним з найбільш простих та інтуїтивно зрозумілих. Однак можна використовувати і інші методи, такі як `sprintf` або різні варіації `printf`. Все залежить від вашої задачі.

## Див. також:

1. [Руководство по Bash от GNU](https://www.gnu.org/software/bash/manual/bash.html)
2. [Стрічкова інтерполяція в Bash | StackOverflow](https://stackoverflow.com/questions/415403/whats-the-best-way-to-ensure-only-non-special-characters-are-in-a-bash-variable)
3. [Використання printf в Bash](https://www.cyberciti.biz/faq/unix-printf-examples/)