---
title:                "C++: Розбір html"
simple_title:         "Розбір html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-html.md"
---

{{< edit_this_page >}}

Висновок: Чому парсінг HTML є важливим для програмістів?

Цей пост присвячений розгляданню парсінгу HTML і тому, чому він є таким важливим для програмістів. Ми розглянемо, як це працює і як ви можете використовувати його у своїх проектах.

## Чому

Парсінг HTML - це процес отримання структурованих даних з HTML-документу. Це важливий етап при роботі з веб-даними, так як багато інтернет-ресурсів надають інформацію саме у форматі HTML. Завдяки парсінгу ми можемо легко отримати необхідні дані з цих ресурсів і використовувати їх у своїх проектах.

## Як використовувати

Для початку, ми повинні завантажити HTML-документ за допомогою бібліотеки, наприклад, ```libcurl```. Після цього ми можемо використовувати функції парсера, такі як ```libxml``` або ```pugixml```, щоб отримати дані з нашого HTML-документу.

```C++
#include <curl/curl.h>
#include <libxml/HTMLparser.h>
#include <pugixml.hpp>

int main() {
    // завантаження HTML-документу
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL,
          "https://www.example.com/");
        res = curl_easy_perform(curl);
    }

    // використання libxml для парсінгу документу
    htmlDocPtr doc;
    doc = htmlReadMemory(res, strlen(res), "", NULL, HTML_PARSE_NOPASSWORD);
    xmlNode* rootElement = xmlDocGetRootElement(doc);
    const xmlChar* xpath = (xmlChar*) "//a";
    xmlXPathObject* result = xmlXPathEvalExpression(xpath, doc);
    if (result != NULL) {
        xmlNodeSet* nodes = result->nodesetval;
        for (int i = 0; i < nodes->nodeNr; i++) {
            // обробка результатів
            printf("%s\n", nodes->nodeTab[i]->children->content);
        }
    }

    // використання pugixml для парсінгу документу
    pugi::xml_document doc;
    doc.load_buffer(res, strlen(res));
    pugi::xml_node rootElement = doc.child("html");

    // обробка результатів
    for (pugi::xml_node a = rootElement.child("a"); a; a = a.next_sibling("a")) {
        printf("%s\n", a.child_value());
    }
}
```

Вихідні дані можуть виглядати так:

```
Home
About
Contact
```

## Deep Dive

Існує багато бібліотек та інструментів для парсінгу HTML у C++, кожен з яких має свої переваги та недоліки. Наприклад, бібліотека ```libxml``` є більш стабільною та має широкий функціонал, але у використанні є дещо складнішою. У свою чергу, ```pugixml``` є більш простою у використанні, але може працювати не так е