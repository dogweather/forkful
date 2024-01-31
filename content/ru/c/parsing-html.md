---
title:                "Разбор HTML"
date:                  2024-01-29T00:01:00.705457-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Разбор HTML означает чтение и понимание структуры HTML-документов программой. Программисты делают это для того, чтобы манипулировать, извлекать или проверять содержимое, часто при скрейпинге веб-сайтов или обработке веб-данных.

## Как:

Хорошо, перейдем к коду. В C нет встроенной поддержки для разбора HTML, поэтому мы будем использовать библиотеку под названием Gumbo, которая является чистым C HTML5 парсером. Вот быстрый пример:

```C
#include <stdio.h>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
       (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        printf("Ссылка найдена: %s\n", href->value);
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(children->data[i]);
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Example</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Пример вывода:

```
Ссылка найдена: https://example.com
```

Этот пример находит теги 'a' и выводит атрибуты href. Не забудьте связать с gumbo (`gcc -o example example.c -lgumbo`) и сначала установить библиотеку.

## Подробнее

История разбора HTML в C немного неровная. Нет универсального решения, потому что HTML сложен и обычно не так уж и однороден. Gumbo, который мы использовали, был разработан Google как часть их проектов с открытым исходным кодом. Он разработан для того, чтобы терпимо относиться к реальному беспорядку веб-страниц.

К альтернативам относятся libxml2 с режимом парсера HTML, хотя исторически он больше был настроен на разбор XML. Другой вариант - htmlcxx, который на самом деле C++, но не будем углубляться.

С точки зрения производительности, C парсеры могут быть очень быстрыми, но обычно не предлагают такого удобства использования, как библиотеки Python. Когда вы используете C для разбора HTML, вы, скорее всего, нацелены на производительность, или вы интегрируете это в существующую кодовую базу на C. Это может быть кропотливой задачей, так как большинство библиотек на C низкоуровневые и требуют большей ручной работы, чем парсеры на Python или JavaScript.

## Смотрите также

- Gumbo Parser: [https://github.com/google/gumbo-parser](https://github.com/google/gumbo-parser)
- HTML-парсер libxml2: [http://xmlsoft.org/html/libxml-HTMLparser.html](http://xmlsoft.org/html/libxml-HTMLparser.html)
- htmlcxx: [http://htmlcxx.sourceforge.net/](http://htmlcxx.sourceforge.net/)
- Для легкого старта рассмотрите возможность изучить учебник по веб-скрейпингу на Python с использованием Beautiful Soup или `html.parser` Python как более простое введение в тему.
