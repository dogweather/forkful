---
title:                "Поиск и замена текста"
aliases:
- ru/python/searching-and-replacing-text.md
date:                  2024-01-29T00:02:23.856833-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Поиск и замена текста заключается в нахождении строк в текстовом блоке и их изменении на что-то другое. Программисты делают это для редактирования кода, обработки данных или автоматизации задач рефакторинга.

## Как это сделать:
```Python
# Использование str.replace() для простой замены
text = "Мне нравится Python. Python - это здорово!"
text = text.replace("Python", "программирование")
print(text)  # Вывод: Мне нравится программирование. Программирование - это здорово!

# Использование re.sub() для замены по шаблону с регулярными выражениями
import re
text = "Связаться с нами можно по адресу support@example.com"
new_text = re.sub(r'\b[a-zA-Z0-9.-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}\b', 'support@newdomain.com', text)
print(new_text)  # Вывод: Связаться с нами можно по адресу support@newdomain.com
```

## Погружение в детали
В ранние дни программирования редактирование текста было ручной работой. Введите регулярные выражения (regex), созданные в 1950-х годах, которые сделали поиск менее мучительным. Для простых замен используйте `str.replace()`. Это просто и отлично подходит для единичных замен. Когда у вас есть шаблоны, такие как номера телефонов, электронные адреса или даты, `re.sub()` с regex становится волшебной палочкой. Он находит шаблоны с помощью специального синтаксиса и заменяет их. Имейте в виду, что regex может быть таким же капризным, как и могущественным; это инструмент, в котором вы становитесь лучше с каждой решенной задачей.

## См. также
- [Документация по `str.replace()` в Python](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Документация по модулю `re` в Python](https://docs.python.org/3/library/re.html)
- [Regex101](https://regex101.com/): Для онлайн-тестирования шаблонов regex
- [Automate the Boring Stuff with Python](https://automatetheboringstuff.com/): Книга, где вы можете узнать больше о практических задачах обработки текста.
