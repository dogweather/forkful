---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/parsing-html.md"
---

{{< edit_this_page >}}

## Що і чому?

Парсинг HTML - це процес, при якому ми аналізуємо структуру HTML-документа та перетворюємо його на машинозрозумілий вигляд. Програмісти роблять це, щоб забезпечити динамічну взаємодію з веб-сторінками та автоматизувати збір даних.

## Як це зробити?

Перед тим, як ми розпочнемо, нам потрібна бібліотека для парсинга HTML, наприклад, Gumbo. 

Встановіть Gumbo:

```bash
sudo apt-get install libgumbo-dev
```

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
        printf("%s\n", href->value);
    }

    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(children->data[i]);
    }
}

int main() {
    GumboOutput* output = gumbo_parse("<a href='http://example.com'>Hello, world!</a>");
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

Цей код шукає посилання в HTML і виводить їх.

## Занурення в глибину

*Історичний контекст:* Раніше парсинг HTML був виключно юрисдикцією веб-браузерів. Однак, із зростанням потреби в автоматизації доступу до веб-контенту, парсинг став загальною практикою.

*Альтернативи:* Інші мови ​​програмування, такі як Python та JavaScript, мають власні бібліотеки для парсингу HTML, як BeautifulSoup та Cheerio відповідно. А HTML документи також можуть бути спарсені за допомогою регулярних виразів, хоча це не є оптимальним рішенням.

*Деталі реалізації:* Парсинг HTML зазвичай вимагає використання DOM (Document Object Model) для навігації по елементах веб-сторінки.

## Дивіться також 

1. [Офіційна документація Gumbo](https://github.com/google/gumbo-parser)
3. [DOM у веб API](https://developer.mozilla.org/uk/docs/Web/API/Document_Object_Model) 
4. [Основи BeautifulSoup в Python](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) 
5. [Швидкий початок з Cheerio в Javascript](https://cheerio.js.org/)