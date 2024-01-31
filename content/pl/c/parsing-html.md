---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:30:12.326301-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML, czyli analiza kodu HTML, jest procesem ekstrakcji danych z dokumentów HTML. Programiści to robią, aby móc korzystać z zawartości stron internetowych w aplikacjach, przetwarzać informacje czy generować struktury danych.

## How to:
Uwaga: C nie jest najbardziej naturalnym językiem do parsowania HTML, ale można tego dokonać używając biblioteki `libxml2`. Oto przykład:

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    htmlDocPtr doc;
    htmlNodePtr node;
	
    // Załadowanie dokumentu HTML
    doc = htmlReadFile("example.html", NULL, 0);
    if (doc == NULL) {
        printf("Could not parse the HTML file\n");
        return 1;
    }
	
    // Dostęp do głównego węzła
    node = xmlDocGetRootElement(doc);
	
    // Iterowanie po węzłach dokumentu
    for (node = node->children; node; node = node->next) {
        if (node->type == XML_ELEMENT_NODE) {
            printf("node type: Element, name: %s\n", node->name);
        }
    }
	
    // Sprzątanie
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return 0;
}
```

Sample output:
```
node type: Element, name: body
node type: Element, name: div
node type: Element, name: p
...
```

## Deep Dive
Parsowanie HTML w C ma swoje korzenie w czasach, gdy dostęp do narzędzi było ograniczony. Libxml2 jest najbardziej zaawansowaną biblioteką w C do tej pracy. Alternatywnie, do innych języków jak Python czy JavaScript istnieją gotowe, wyspecjalizowane biblioteki.

Jako że HTML jest często nieregularny i może zawierać błędy, każdy parser musi być odporny na niepoprawne dane. Libxml2 radzi sobie z takimi przypadkami, normalizując HTML przed analizą.

Zaangażowanie C w procesie parsowania jest sensowne przy potrzebie szybkości lub integracji z istniejącym kodem w C. Znaczące są także ograniczenia – w C jest więcej kodu "boilerplate" i konieczna jest obsługa pamięci, której inne języki zarządzają automatycznie.

## See Also
- Dokumentacja libxml2: http://xmlsoft.org/html/libxml-HTMLparser.html
- Tutorial do libxml2: http://xmlsoft.org/tutorial/index.html
- Alternatywy dla C - BeautifulSoup dla Python: https://www.crummy.com/software/BeautifulSoup/
- jsoup: Biblioteka do parsowania HTML w Java: https://jsoup.org/
