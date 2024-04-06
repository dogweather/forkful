---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Python \u043C\
  \u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u0439 \u043C\
  \u0435\u0442\u043E\u0434 `.capitalize()` \u0434\u043B\u044F \u0440\u044F\u0434\u043A\
  \u0456\u0432, \u0449\u043E \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043B\
  \u0435\u0433\u043A\u043E \u0432\u0438\u043A\u043E\u043D\u0430\u0442\u0438 \u0446\
  \u0435 \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F."
lastmod: '2024-04-04T00:27:28.380432-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u041F\u0440\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0430 \u0434\u043E \u0432\u0435\u0440\u0445\u043D\u044C\u043E\u0433\u043E\
  \ \u0440\u0435\u0433\u0456\u0441\u0442\u0440\u0443"
weight: 2
---

## Як зробити:

### Використовуючи Вбудований Метод Python:
Python має вбудований метод `.capitalize()` для рядків, що дозволяє легко виконати це завдання.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Вивід:**
```
Hello world
```

Ось моя власна налаштована функція `capitalize()`, яку я використовую для створення цього сайту. Мені потрібно було переконатися, що спеціальні слова як **HTML** завжди залишаються написаними великими літерами. Це також демонструє [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Зробити першу літеру рядка великою.
    Обробити спеціальні випадки, як "HTML".

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```

### Обробка Декількох Слів:
Для сценаріїв, коли ви хочете, щоб кожне слово в рядку починалося з великої літери (як в заголовках), можна застосувати метод `.title()`.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Вивід:**
```
Python Programming Essentials
```

### Використання Бібліотек Третьої Сторони:
Хоча стандартна бібліотека Python оснащена для базового написання рядків з великої літери, бібліотеки як `textblob` можуть пропонувати більш тонкий контроль, особливо для обробки природної мови.

Спочатку переконайтеся, що у вас встановлений `textblob`:
```bash
pip install textblob
```

Далі використовуйте його для написання рядків з великої літери, пам'ятайте, що `textblob` може працювати по-різному залежно від контексту використання:

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

Пам'ятайте, хоча методи `capitalize()` і `title()` є універсально корисними, використання бібліотек як `textblob` може забезпечити додаткову гнучкість для специфічних застосувань.
