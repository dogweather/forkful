---
title:                "Работа с XML"
aliases:
- ru/fish-shell/working-with-xml.md
date:                  2024-01-29T00:04:59.587071-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML означает манипуляции с данными в повсеместно используемом, структурированном формате, применимом в настройках, мессенджерах и многом другом. Программисты управляют XML для чтения, записи, обновления и запроса данных—что жизненно важно для взаимодействия множества приложений и сервисов.

## Как:
У Fish нет встроенного парсинга XML, так что вам придется опираться на внешние инструменты вроде `xmllint` или `xmlstarlet`. Вот пример чтения значений:

```fish
# Парсинг XML с использованием xmlstarlet
echo '<root><element>Привет Мир</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Вывод:
```
Привет Мир
```

Для редактирования XML используйте следующее:

```fish
# Редактирование элемента XML с использованием xmlstarlet
echo '<root><element>Старое Значение</element></root>' | xmlstarlet ed -u "/root/element" -v 'Новое Значение'
```

Вывод:
```xml
<?xml version="1.0"?>
<root>
  <element>Новое Значение</element>
</root>
```

## Подробнее:
XML существует с конца 90-х, созданный для удобочитаемости и дружелюбности к машинам. Несмотря на то что JSON отнял у XML часть популярности благодаря простоте, XML остается незаменимым там, где ключевыми являются валидация документов и пространства имён.

Альтернативы? Конечно — JSON, YAML или даже бинарные форматы вроде Protocol Buffers для приложений, требовательных к производительности. Но схема XML и XSLT (для трансформации XML) могут быть решающими для сложных сценариев, где важна надёжность.

Под капотом инструменты вроде `xmlstarlet` используют мощные библиотеки, такие как libxml2, предоставляя вам XPath и XQuery для детальной работы с XML. Это не только инструменты для работы с XML, но и ключи к манипуляции с DOM, так как вы применяете аналогичные концепции в любом языке, работающем с XML.

## Смотрите также:
- [Документация xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Документация Fish](https://fishshell.com/docs/current/index.html)
- [Функции и операторы XPath и XQuery](https://www.w3.org/TR/xpath-functions/)
