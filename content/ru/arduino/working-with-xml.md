---
title:                "Работа с XML"
date:                  2024-01-29T00:04:42.624868-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML на Arduino включает в себя анализ и манипуляцию данными XML, которые обычно поступают из веб-API или файлов конфигурации. Программисты делают это для интеграции с сервисами, использующими XML для обмена данными, или для хранения данных в структурированном, удобочитаемом формате.

## Как:
Мы будем использовать библиотеку `XMLWriter` для создания XML и библиотеку `tinyxml2` для его анализа. Установите библиотеки через Менеджер библиотек в вашей среде разработки Arduino IDE.

Создание XML-документа:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Используем Serial для вывода
  
  xml.header();
  xml.tag("greeting").tag("text").text("Привет, мир!").close().close();
  xml.flush();
}

void loop() {
}
```

Декодирование XML-строки:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Привет, мир!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Пример вывода:

```
<greeting>
  <text>Привет, мир!</text>
</greeting>
```

## Подробнее
XML или Расширяемый Язык Разметки - это язык разметки, который определяет набор правил для кодирования документов в формате, который является как удобочитаемым, так и машиночитаемым. Он существует с конца 90-х и широко используется в различных областях, особенно там, где необходим обмен данными, не зависящий от платформы. Ограниченные ресурсы памяти Arduino делают работу с XML более сложной, чем на ПК. Поэтому крайне важны легковесные библиотеки. Хотя JSON получил популярность для обмена данными из-за своего более простого синтаксиса и меньшего размера, XML все еще широко используется, особенно при работе с устаревшими системами или приложениями, требующими проверки документов через схемы. Ключом к реализации XML на Arduino является поточный парсинг, который считывает документ по частям для снижения использования памяти.

## Смотрите также
- [Документация по библиотеке TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [Библиотека Arduino JSON](https://arduinojson.org/) как альтернатива при работе с данными JSON.
- [XML учебник от W3Schools](https://www.w3schools.com/xml/) для общего изучения XML.
- [Спецификация XML от W3C](https://www.w3.org/XML/) для официальных стандартов и рекомендаций XML.
