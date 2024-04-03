---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:30.940216-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : #."
lastmod: '2024-03-13T22:44:48.555946-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0417\u0440\u043E\u0431\u0438\u0442\u0438 \u043F\u0435\u0440\u0448\u0443\
  \ \u043B\u0456\u0442\u0435\u0440\u0443 \u0440\u044F\u0434\u043A\u0430 \u0432\u0435\
  \u043B\u0438\u043A\u043E\u044E"
weight: 2
---

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
