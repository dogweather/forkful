---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: \u0412\
  \ Python \u0435\u0441\u0442\u044C \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\
  \u044B\u0439 \u043C\u0435\u0442\u043E\u0434 `.capitalize()` \u0434\u043B\u044F \u0441\
  \u0442\u0440\u043E\u043A, \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0449\
  \u0438\u0439 \u043B\u0435\u0433\u043A\u043E \u0441\u043F\u0440\u0430\u0432\u0438\
  \u0442\u044C\u0441\u044F \u0441 \u044D\u0442\u043E\u0439 \u0437\u0430\u0434\u0430\
  \u0447\u0435\u0439."
lastmod: '2024-04-04T00:26:57.698567-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 2
---

## Как сделать:


### Использование встроенного метода Python:
В Python есть встроенный метод `.capitalize()` для строк, позволяющий легко справиться с этой задачей.

```python
my_string = "привет мир"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Вывод:**
```
Привет мир
```

Вот мой собственный настроенный метод `capitalize()`, который я использую для создания этого сайта. Мне нужно было убедиться, что особые слова, например **HTML**, всегда остаются заглавными. Это также демонстрирует использование [doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Сделать первую букву строки заглавной.
    Обрабатывать особые случаи, например "HTML".

    >>> capitalize("это html, csv, xml и http (без REPL).")
    'Это HTML, CSV, XML и HTTP (без REPL).'

    >>> capitalize("это json, VBA, IDE и yaml в CLI.")
    'Это JSON, VBA, IDE и YAML в CLI.'
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




### Обработка нескольких слов:
Для ситуаций, когда вы хотите, чтобы каждое слово в строке начиналось с заглавной буквы (например, в заголовках), можно применить метод `.title()`.

```python
my_title = "основы программирования на python"
title_case = my_title.title()
print(title_case)
```
**Вывод:**
```
Основы Программирования На Python
```

### Использование сторонних библиотек:
Хотя стандартная библиотека Python оснащена для базовой капитализации строк, библиотеки, такие как `textblob`, могут предложить более тонкий контроль, особенно для обработки естественного языка.

Сначала убедитесь, что у вас установлен `textblob`:
```bash
pip install textblob
```

Затем используйте его для капитализации строк, имея в виду, что капитализация в `textblob` может работать по-разному в зависимости от контекста использования:

```python
from textblob import TextBlob

my_sentence = "это тестовое предложение"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Вывод:**
```
Это тестовое предложение
```

Помните, хотя методы `capitalize()` и `title()` универсально полезны, использование библиотек, таких как `textblob`, может предоставить дополнительную гибкость для конкретных приложений.
