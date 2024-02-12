---
title:                "Аналіз HTML"
aliases:
- /uk/cpp/parsing-html/
date:                  2024-02-03T19:12:01.262218-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Аналіз HTML полягає в розбитті HTML-вмісту на щось, що програма може розуміти та маніпулювати. Програмісти роблять це для витягу даних, маніпуляцій з вмістом або інтеграції веб-скрапінгу в свої додатки.

## Як це зробити:
C++ не має вбудованих можливостей для аналізу HTML. Зазвичай ви використовуєте бібліотеку, як-от Gumbo-parser від Google, або щось подібне. Ось швидкий приклад використання Gumbo-parser:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Приклад виводу:
```
https://example.com
```

## Поглиблений аналіз
Аналіз HTML завжди не був простим завданням в C++. Історично програмісти використовували regex або написані вручну парсери, обидва з яких схильні до помилок і громіздкі. На сьогодні, надійні бібліотеки, як-от Gumbo-parser, вирішують складнощі парсингу, роблячи це легшим і надійнішим.

Альтернативи включають Tidy, MyHTML, або навіть інтеграцію C++ з Python's BeautifulSoup через функцію `system` C++ або вбудовані інтерпретатори.

З точки зору імплементації, ці бібліотеки конвертують HTML в дерево Document Object Model (DOM). Проходження та маніпуляція з DOM дозволяє користувачам витягувати та працювати з даними, як показано в розділі Як це зробити.

## Дивіться також
- [Репозиторій Gumbo-parser на GitHub](https://github.com/google/gumbo-parser)
- [Список бібліотек для аналізу HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Взаємодія C++ та Python](https://docs.python.org/3/extending/embedding.html)
