---
title:                "Работа с XML"
date:                  2024-01-29T00:04:59.075601-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML в C включает в себя разбор, создание и манипулирование XML-файлами - по сути, структурированным хранением данных. Программисты делают это для взаимодействия с данными в переносимом и читаемом человеком формате, часто используемом для конфигурации, обмена данными и многого другого.

## Как это сделать:
Ниже приведен фрагмент кода, использующий библиотеку `libxml2` для разбора XML-файла и получения корневого элемента.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Разбор XML-файла
    doc = xmlReadFile("example.xml", NULL, 0);

    // Получение корневого элемента
    root_element = xmlDocGetRootElement(doc);

    printf("Корневой Элемент: %s\n", root_element->name);

    // Освобождение документа
    xmlFreeDoc(doc);

    // Очистка парсера
    xmlCleanupParser();

    return 0;
}
```

Пример вывода для XML с корнем `<data>` может быть:
```
Корневой Элемент: data
```

## Глубокое погружение
XML или Расширяемый язык разметки, появился в конце 90-х годов, предоставляя способ описания и структурирования данных. В C `libxml2` является основным выбором. Эта библиотека надежна, хотя и не самая простая для начинающих с XML. Альтернативы включают `tinyxml2`, которая легче и более подходяща для начинающих. Что касается реализации, C не имеет встроенной поддержки XML, поэтому библиотеки заполняют этот пробел. Они различаются по размеру, скорости, сложности и переносимости. Большинство предлагают методы разбора DOM и SAX: DOM загружает всё в память, хорошо для малых документов; SAX работает на основе событий, обрабатывая элементы на лету, лучше для больших файлов. У обоих есть свои случаи использования и компромиссы.

## Смотрите также
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 на GitHub](https://github.com/leethomason/tinyxml2)
- [Учебник по XML на w3schools](https://www.w3schools.com/xml/)
- [Спецификация XML от W3C](https://www.w3.org/XML/)