---
title:                "Разбор HTML"
aliases:
- /ru/cpp/parsing-html/
date:                  2024-01-28T23:59:51.146714-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Парсинг HTML означает разбор HTML-контента на части, которые программа может понять и обработать. Программисты делают это для извлечения данных, манипуляции с содержимым или интеграции веб-скрепинга в свои приложения.

## Как:
C++ изначально не имеет встроенных возможностей для парсинга HTML. Чаще всего используется библиотека, например, Gumbo-parser от Google или что-то подобное. Вот короткий пример с использованием Gumbo-parser:

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
    const char* html = "<html><body><a href='https://example.com'>Ссылка</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Пример вывода:
```
https://example.com
```

## Подробнее
Парсинг HTML никогда не был прост в C++. Исторически программисты использовали regex или написанные вручную парсеры, оба из которых подвержены ошибкам и громоздки. Сегодня надежные библиотеки, такие как Gumbo-parser, обрабатывают сложности парсинга, делая его проще и надежнее.

Альтернативами являются Tidy, MyHTML, или даже интеграция C++ с Python's BeautifulSoup через функцию `system` C++ или встроенные интерпретаторы.

С точки зрения реализации, эти библиотеки конвертируют HTML в дерево объектной модели документа (DOM). Перемещение и манипуляция DOM позволяет пользователям извлекать и работать с данными, как это демонстрируется в разделе Как.

## См. также
- [Репозиторий Gumbo-parser на GitHub](https://github.com/google/gumbo-parser)
- [Список библиотек для парсинга HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Взаимодействие C++ и Python](https://docs.python.org/3/extending/embedding.html)
