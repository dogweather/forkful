---
title:                "Работа с XML"
date:                  2024-01-29T00:04:42.170271-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML включает в себя разбор, извлечение и манипулирование данными в формате Extensible Markup Language. Программисты сталкиваются с XML, поскольку это широко распространенный формат обмена данными для конфигураций, API и многого другого.

## Как это сделать:
Вот как разобрать XML в Bash. Инструменты? xmllint и xmlstarlet. Циклическое прохождение через элементы XML? Безусловно. Пример с примером вывода:

```bash
# Предполагаем, что xmlstarlet установлен
# Установить при помощи: apt-get install xmlstarlet

# Разбор содержимого XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Извлечение имен при помощи xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# Вывод должен быть:
# Apple
# Banana
```

## Глубокое погружение
В 90-х годах XML появился как более простая альтернатива SGML, но более структурированная, чем HTML. Теперь у него есть компания – например, JSON, YAML. Но XML все еще на коне, особенно в конфигурациях и веб-сервисах, основанных на SOAP.

Что касается инструментария, xmllint удобен для валидации XML, запросов xpath. xmlstarlet - это швейцарский нож для шалостей с XML – запросы, редактирование, валидация, трансформация. В bash-скриптах они являются супергероями для задач по XML.

Под капотом у xmllint используется libxml2 – C-парсер XML. Он быстрый, но сообщения об ошибках? Загадочные. А xmlstarlet? Рекурсивные шаблоны и поддержка EXSLT. Головоломка, но мощная.

## Смотрите также
- [xmlsoft.org](http://xmlsoft.org/): Материалы по Libxml2 и xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Реальные проблемы и решения.
- [W3Schools Учебник по XML](https://www.w3schools.com/xml/): Основы XML.
