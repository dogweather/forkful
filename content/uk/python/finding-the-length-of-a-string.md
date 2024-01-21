---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:48:01.655742-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Рахувати довжину рядка в Python - це як виміряти його від кінця до краю. Програмісти це роблять, щоб керувати текстовими даними: розуміти їх розмір, порівнювати, чи обмежувати ввід.

## Як це зробити:
Простий спосіб - використовуйте `len()`:

```python
# Приклад 1: Просте вимірювання
message = "Привіт, світ!"
length = len(message)
print(length)

# Вивід: 13
```

Якщо потрібно проаналізувати кожен символ:

```python
# Приклад 2: Цикл для рахування
message = "Доброго ранку"
count = 0
for character in message:
    count += 1
print(count)

# Вивід: 13
```

## Поглиблений аналіз:
Історично, функція `len()` в Python завжди була швидкою та простою у використанні. Вона використовує об'єкт `__len__` під капотом.

Альтернативи? Так, іноді користувачі пишуть власні функції, але це зазвичай для особливих випадків.

Імплементація? Python зберігає інформацію про довжину в даних об'єкта, тому `len()` працює майже миттєво.

## Дивіться також:
- Документація Python про [`len()`](https://docs.python.org/3/library/functions.html#len)
- [Стрічки у Python](https://docs.python.org/3/tutorial/introduction.html#strings) - щоб зрозуміти основи роботи з текстовими даними.