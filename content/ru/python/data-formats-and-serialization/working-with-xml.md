---
title:                "Работа с XML"
aliases: - /ru/python/working-with-xml.md
date:                  2024-01-29T00:05:15.528440-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
"Работа с XML" относится к процессу чтения, создания и изменения файлов XML (eXtensible Markup Language) с использованием программирования. Программисты делают это, потому что XML широко используется для обмена данными благодаря его платформонезависимому характеру и самоописываемому формату.

## Как:
Модуль Python `xml.etree.ElementTree` предлагает инструменты для работы с XML.

Разбор XML-документа:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<библиотека>
    <книга>
        <название>Изучаем Python</название>
        <автор>Марк Лутц</автор>
    </книга>
    <книга>
        <название>Программирование на Python</название>
        <автор>Марк Лутц</автор>
    </книга>
</библиотека>
"""

root = ET.fromstring(xml_data)
for book in root.findall('книга'):
    title = book.find('название').text
    author = book.find('автор').text
    print(f'Название: {title}, Автор: {author}')
```
Пример вывода:
```
Название: Изучаем Python, Автор: Марк Лутц
Название: Программирование на Python, Автор: Марк Лутц
```

Создание XML-документа:
```python
library = ET.Element('библиотека')
book = ET.SubElement(library, 'книга')
title = ET.SubElement(book, 'название')
title.text = 'Автоматизация скучной работы с помощью Python'
author = ET.SubElement(book, 'автор')
author.text = 'Ал Свейгарт'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## Подробнее:
XML существует с конца 90-х годов, созданный как упрощенное подмножество SGML для удобного обмена данными онлайн. Несмотря на растущую популярность JSON для веб-данных, XML остаётся жизненно важным во многих предприятиях, конфигурациях и веб-сервисах (SOAP, RSS).

Альтернативы `xml.etree.ElementTree` включают в себя `lxml` и `minidom`. `lxml` работает быстрее и имеет больше функций, в то время как `minidom` предоставляет более "DOM-подобный" интерфейс XML. При выборе учитывайте удобство использования, производительность и конкретные требования к функционалу.

Под капотом `ElementTree` работает на модели дерева элементов, где каждый компонент XML-файла является узлом в дереве. Это позволяет использовать простые выражения путей и поиски, делая навигацию и манипуляцию структурой данных XML проще.

## См. также:
- Модуль Python `xml.etree.ElementTree`: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- Учебник XML от W3Schools: https://www.w3schools.com/xml/
- Спецификация XML: https://www.w3.org/XML/
