---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:51.146714-07:00
description: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\u043E\u0440 HTML-\u043A\u043E\u043D\
  \u0442\u0435\u043D\u0442\u0430 \u043D\u0430 \u0447\u0430\u0441\u0442\u0438, \u043A\
  \u043E\u0442\u043E\u0440\u044B\u0435 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0430 \u043C\u043E\u0436\u0435\u0442 \u043F\u043E\u043D\u044F\u0442\u044C \u0438\
  \ \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u0430\u0442\u044C. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\
  \u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u0438\u0437\u0432\u043B\u0435\u0447\
  \u0435\u043D\u0438\u044F \u0434\u0430\u043D\u043D\u044B\u0445,\u2026"
lastmod: '2024-03-13T22:44:45.603918-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\u043E\u0440 HTML-\u043A\u043E\u043D\
  \u0442\u0435\u043D\u0442\u0430 \u043D\u0430 \u0447\u0430\u0441\u0442\u0438, \u043A\
  \u043E\u0442\u043E\u0440\u044B\u0435 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0430 \u043C\u043E\u0436\u0435\u0442 \u043F\u043E\u043D\u044F\u0442\u044C \u0438\
  \ \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u0430\u0442\u044C. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\
  \u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u0438\u0437\u0432\u043B\u0435\u0447\
  \u0435\u043D\u0438\u044F \u0434\u0430\u043D\u043D\u044B\u0445,\u2026"
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
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
