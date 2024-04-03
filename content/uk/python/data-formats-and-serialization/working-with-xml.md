---
date: 2024-01-26 04:35:19.666028-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041C\u043E\u0434\u0443\u043B\u044C `xml.etree.ElementTree` Python \u043F\u0440\
  \u043E\u043F\u043E\u043D\u0443\u0454 \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\
  \u043D\u0442\u0438 \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437\
  \ XML. \u0420\u043E\u0437\u0456\u0431\u0440\u0430\u0442\u0438 XML-\u0434\u043E\u043A\
  \u0443\u043C\u0435\u043D\u0442."
lastmod: '2024-03-13T22:44:48.622665-06:00'
model: gpt-4-0125-preview
summary: "\u041C\u043E\u0434\u0443\u043B\u044C `xml.etree.ElementTree` Python \u043F\
  \u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u0456\u043D\u0441\u0442\u0440\u0443\u043C\
  \u0435\u043D\u0442\u0438 \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438\
  \ \u0437 XML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як це зробити:
Модуль `xml.etree.ElementTree` Python пропонує інструменти для роботи з XML.

Розібрати XML-документ:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'Назва: {title}, Автор: {author}')
```
Приклад виведення:
```
Назва: Learning Python, Автор: Mark Lutz
Назва: Programming Python, Автор: Mark Lutz
```

Створити XML-документ:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## Поглиблений розгляд:
XML існує з кінця 90-х років, створений як спрощений підмножина SGML для легкого онлайн обміну даними. Незважаючи на зростаючу популярність JSON для веб-даних, XML залишається важливим у багатьох підприємницьких, конфігураційних та веб-сервісах (SOAP, RSS).

Альтернативи до `xml.etree.ElementTree` включають `lxml` та `minidom`. `lxml` швидший і має більш багатий набір функцій, в той час як `minidom` забезпечує більш "DOM-подібний" XML інтерфейс. При виборі варто враховувати простоту використання, продуктивність та специфічні вимоги до функціоналу.

Під капотом, `ElementTree` працює на моделі дерева елементів, де кожний компонент XML-файлу є вузлом у дереві. Це дозволяє використовувати прості вирази шляху і пошуку, роблячи навігацію та маніпуляцію структурою даних XML легшою.

## Дивись також:
- Модуль Python `xml.etree.ElementTree`: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- XML-посібник від W3Schools: https://www.w3schools.com/xml/
- Специфікація XML: https://www.w3.org/XML/
