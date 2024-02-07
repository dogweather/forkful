---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:35:44.234393-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Об'єднання рядків – це спосіб зшивання їх в один довгий рядок. Робимо це, щоб збирати текст з частин, формувати повідомлення, шляхи файлів, SQL запити та інше.

## How to (Як це робити):
Щоб об'єднати рядки у Python, можемо використовувати `+`, функцію `join()`, або f-strings.

```python
# Використання +
hello = "Привіт, "
world = "світе!"
greeting = hello + world
print(greeting)
# Вивід: Привіт, світе!

# Використання join()
words = ["Python", "це", "чудово"]
sentence = " ".join(words)
print(sentence)
# Вивід: Python це чудово

# Використання f-strings
first_name = "Іван"
last_name = "Петрович"
full_name = f"{first_name} {last_name}"
print(full_name)
# Вивід: Іван Петрович
```

## Deep Dive (Занурення в глибину):
#### Історичний контекст:
У ранніх версіях Python для об'єднання рядків частіше використовували операцію `+`. З часом з'явилися f-strings та метод `join()`, які надали більше можливостей та ефективності.

#### Альтернативи:
- `%` для форматування строк, але вже застарілий;
- `format()` метод, універсальний, але менш зручний ніж f-strings.

#### Реалізація:
Коли використовуємо `+`, Python створює новий рядок з кожним додаванням, що не завжди ефективно. Метод `join()` вважається ідеальним для об'єднання багатьох рядків, оскільки він спочатку розмірює місце, що треба, а потім вставляє елементи. F-strings вважаються найзручнішими для читання та запису, а також вони достатньо швидкі.

## See Also (Дивіться також):
- [Документація по роботі з рядками у Python](https://docs.python.org/3/library/string.html)
- [PEP 498 -- Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/)
- [Python String Formatting Best Practices](https://realpython.com/python-string-formatting/)
