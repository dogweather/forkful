---
title:                "Зробити першу літеру рядка великою"
aliases:
- /uk/python/capitalizing-a-string.md
date:                  2024-02-03T19:06:30.940216-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Почати рядок з великої літери означає перетворення першого символу рядка на велику букву, а решту - на маленькі літери. Ця операція часто використовується в обробці даних для нормалізації вхідних даних або підвищення читабельності для заголовків, імен та подібного.

## Як це зробити:

### Використання вбудованого методу Python:
Python має вбудований метод `.capitalize()` для рядків, щоб легко виконати це завдання.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Вивід:**
```
Hello world
```

### Обробка декількох слів:
Для ситуацій, коли ви хочете, щоб кожне слово в рядку починалося з великої літери (наприклад, заголовки), можна застосувати метод `.title()`.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Вивід:**
```
Python Programming Essentials
```

### Використання сторонніх бібліотек:
Хоча стандартна бібліотека Python оснащена для базового перетворення рядків з великої літери, бібліотеки на кшталт `textblob` можуть пропонувати більш тонкий контроль, особливо для обробки природної мови.

Спочатку переконайтеся, що у вас встановлено `textblob`:
```bash
pip install textblob
```

Потім використовуйте його для перетворення рядків із заголовної букви, пам'ятайте, що capitalize в `textblob` може працювати по-різному, залежно від контексту використання:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Вивід:**
```
This is a test sentence
```

Пам’ятайте, хоча методи `capitalize()` та `title()` є універсально корисними, використання бібліотек на кшталт `textblob` може забезпечити додаткову гнучкість для конкретних застосувань.
