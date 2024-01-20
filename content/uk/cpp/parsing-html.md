---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Парсинг HTML - це процес видобування специфічної інформації з HTML-документа. Це часто потрібно програмістам для обробки, аналізу та використання даних у HTML-файлах.

## Як це робиться:

Для парсингу HTML у C++ корисно використовувати бібліотеку Gumbo. Ось приклад коду:

```C++
#include "gumbo.h"
...
void parse_html(const char* html) {
    GumboOutput* output = gumbo_parse(html);
    ...
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```
Выдобута інформація потім використовується у програмі.

## Детальніше:

Історично, парсинг HTML був складою та помилкозрону задачею. HTML володіє комплексною структурою та звичайно містить помилки у коді, які не заважають його відображенню у браузері, але утруднюють парсинг.

Альтернативи парсингу HTML на C++ включають використання таких бібліотек, як htmlcxx, myhtml або використання інших мов програмування, таких як Python або JavaScript, які мають вбудовані інструменти для цього.

Що стосується деталей реалізації, Gumbo перетворює HTML в DOM-дерево, яке потім можна проаналізувати та використати.

## Дивись також:

1. Офіційний репозиторій Gumbo на GitHub: https://github.com/google/gumbo-parser.
2. "Чому Gumbo": https://opensource.googleblog.com/2013/08/announcing-gumbo-new-c-html5-parser.html.
3. Вікіпедія про DOM: https://uk.wikipedia.org/wiki/Document_Object_Model.