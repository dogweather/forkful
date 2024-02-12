---
title:                "Склеивание строк"
date:                  2024-01-28T23:56:37.012178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Склеивание строк"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Конкатенация строк означает их склеивание друг с другом для создания новой строки. Это как конструктор из строк. Мы делаем это для формирования текста; подумайте о именах пользователей, сообщениях об ошибках и динамическом контенте.

## Как:
Давайте объединим несколько строк.

```python
first_name = "Charlie"
last_name = "Brown"
full_name = first_name + " " + last_name  # Классическая конкатенация с пробелом
print(full_name)
```
Вывод: `Charlie Brown`

Использование `join()` для списка слов:

```python
words = ["Привет", "мир!"]
sentence = " ".join(words)
print(sentence)
```
Вывод: `Привет мир!`

F-строка (начиная с Python 3.6):

```python
user = "snoopy"
action = "летает"
log_message = f"{user} {action} на своей собачьей будке"
print(log_message)
```
Вывод: `snoopy летает на своей собачьей будке`

## Подробнее
Конкатенация была фундаментальной операцией со строками с зари программирования. Помните, Python относится к строкам как к неизменяемым, так что каждая конкатенация создаёт новую строку.

Когда-то плюс (`+`) был всем, что у нас было. Неэффективно для множества строк, так как это могло привести к перерасходу памяти и замедлению производительности. Вот тут на сцене и появляется метод `join()` — более дружественный к памяти, особенно при слиянии серии строк.

F-строки, введенные в Python 3.6, стали настоящим прорывом. Они читаемы и быстры, и позволяют оценивать выражения внутри строковых литералов — `f"{переменная}"`. Они стали избранным инструментом современного питониста, объединяющим функциональность и эффективность.

## Смотрите также
- [Методы строк в Python](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [PEP 498 -- Литеральная интерполяция строк](https://www.python.org/dev/peps/pep-0498/)
- [Лучшие практики форматирования строк в Python](https://realpython.com/python-f-strings/)