---
title:                "Парсинг HTML"
date:                  2024-01-20T15:30:47.863984-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?

Розбір HTML – це процес аналізу коду HTML для витягування даних або змінення структури. Програмісти парсять HTML, щоб автоматизувати процеси, збирати інформацію з веб-сторінок, чи модифікувати контент.

## Як це зробити:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }

    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
       (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        std::cout << href->value << std::endl;
    }

    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Example</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

Вище наведений код використовує бібліотеку Gumbo для пошуку усіх посилань на сторінці. В результаті ми отримаємо: 

```
https://example.com
```

## Підводні камені:

Парсинг HTML може бути справою хитрою. Структура HTML часто мінлива, а стандарти еволюціонують. У минулому, багато бібліотек для парсингу HTML, на зразок Beautiful Soup у Python, рятували ситуацію, але в C++ такий стандартний інструмент відсутній.

Сьогодні вибір падає на такі бібліотеки, як Gumbo – інтерфейс розроблений Google для досконалого розбору документів HTML5. Ще одна альтернатива – бібліотека htmlcxx, яка менш вимоглива до стандартів HTML і може обробляти більш хаотичний HTML-код.

Реалізуючи парсер HTML, важливо бути готовим до неочікуваних змін у HTML-структурі та робити код максимально адаптивним.

## Дивись також:

- Офіційна сторінка Gumbo Parser: https://github.com/google/gumbo-parser
- Завантаження та інструкції до htmlcxx: http://htmlcxx.sourceforge.net/
- Огляд парсерів HTML для C++: https://www.slant.co/topics/1234/~best-html-parsers-for-cplusplus
