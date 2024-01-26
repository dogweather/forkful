---
title:                "Робота з XML"
date:                  2024-01-26T04:28:34.513676-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Що та Чому?
Робота з XML у C передбачає аналіз, створення та маніпуляції файлами XML - по суті, структурованим сховищем даних. Програмісти роблять це для взаємодії з даними у портативному та легкозрозумілому форматі, який часто використовується для конфігурації, обміну даними та іншого.

## Як:
Нижче наведено фрагмент із використанням бібліотеки `libxml2` для аналізу файлу XML та отримання кореневого елемента.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Аналізуємо файл XML
    doc = xmlReadFile("example.xml", NULL, 0);

    // Отримуємо кореневий елемент
    root_element = xmlDocGetRootElement(doc);

    printf("Кореневий елемент: %s\n", root_element->name);

    // Звільняємо документ
    xmlFreeDoc(doc);

    // Очищення парсера
    xmlCleanupParser();

    return 0;
}
```

Приклад виводу для XML з коренем `<data>` може бути:
```
Кореневий елемент: data
```

## Поглиблене вивчення
XML, або Розширювана мова розмітки, датується кінцем 90-х років, надаючи спосіб опису та структуризації даних. У мові C `libxml2` є перевіреним вибором. Вона надійна, хоча й не найлегша для новачків у XML. Альтернативи включають `tinyxml2`, яка є легшою та більш дружньою для початківців. Що стосується реалізації, C не має вбудованої підтримки XML, тому бібліотеки заповнюють цей прогап. Вони варіюються за розміром, швидкістю, складністю та переносимістю. Більшість пропонують методи аналізу DOM та SAX: DOM завантажує все у пам’ять, гарно для малих документів; SAX працює на подіях, обробляє елементи на льоту, краще для великих файлів. Обидва мають свої випадки використання та компроміси.

## Див. також
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 на GitHub](https://github.com/leethomason/tinyxml2)
- [XML навчальний посібник на w3schools](https://www.w3schools.com/xml/)
- [Специфікація XML від W3C](https://www.w3.org/XML/)