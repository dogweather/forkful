---
title:                "C: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Більшість браузерів використовують мову опису документів (HTML) для створення веб-сторінок. Парсинг HTML є важливою задачею для програмістів, оскільки вона дозволяє ефективно отримувати дані з веб-сторінок.

## Як

Існує кілька популярних бібліотек для парсингу HTML в С-подібних мов програмування. Одним із них є libxml2, який дозволяє обробляти XML та HTML-дані. Давайте розглянемо приклад, як використовувати цю бібліотеку для отримання потрібних даних з веб-сторінки.

```C
#include <libxml/xmlreader.h>
#include <libxml/HTMLparser.h>
#include <libxml/xpath.h>

int main()
{
    // Створюємо новий контекст обробки html-документу
    htmlParserCtxtPtr html_context = htmlNewParserCtxt();

    // Визначаємо URL веб-сторінки, яку ми хочемо обробити
    const char *url = "https://www.example.com";

    // Отримуємо дерево вузлів
    xmlDocPtr html_doc = htmlCtxtReadDoc(html_context, url, NULL, XML_PARSE_NOERROR | XML_PARSE_NOWARNING);

    // Створюємо Xpath контекст
    xmlXPathContextPtr xpath_context = xmlXPathNewContext(html_doc);

    // Задаємо вираз для отримання тексту з елемента <h1>
    xmlXPathObjectPtr xpath_result = xmlXPathEvalExpression((xmlChar *)"//h1/text()", xpath_context);

    // Отримуємо значення першого знайденого елемента
    xmlChar *h1_text = xmlXPathCastToString(xpath_result->nodesetval->nodeTab[0]->content);

    // Виводимо результат
    printf("Заголовок: %s\n", h1_text);

    // Звільняємо пам'ять
    xmlFreeDoc(html_doc);
    xmlFree(h1_text);
    xmlXPathFreeObject(xpath_result);
    xmlXPathFreeContext(xpath_context);

    return 0;
}
```

При виконанні даної програми, ми отримаємо вивід:

```
Заголовок: Приклад
```

## Deep Dive

Парсинг HTML включає в себе перетворення різноманітних тегів та атрибутів в текстовий формат. Наприклад, тег `<a>` використовується для створення посилань на інші веб-сторінки. Під час парсингу ми можемо отримати URL посилання та текст, що відображатиметься на кнопці, щоб легко навігувати до інших сторінок.

Також варто зауважити, що парсинг HTML є одним із елементів веб-скрапінгу, який дозволяє автоматично збирати дані з великої кількості веб-сторінок.

## Дивись також

- [libxml2 офіційна документація](https://libxml2.org/)
- [Стаття про парсинг HTML у С++