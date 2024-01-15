---
title:                "Analizowanie html"
html_title:           "C: Analizowanie html"
simple_title:         "Analizowanie html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Parsing HTML jest ważnym aspektem programowania w C. Pozwala nam na efektywne przetwarzanie i analizę struktur HTML, co jest kluczowe w dzisiejszym świecie internetu. Dzięki temu możemy łatwo korzystać z danych i informacji znajdujących się na stronach internetowych.

## Jak to zrobić

Aby rozpocząć parsowanie HTML w C, możemy użyć biblioteki libxml2, która jest jedną z najpopularniejszych wśród programistów C. Poniżej przedstawiony jest przykładowy kod, który wypisze zawartość tytułu strony internetowej.

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

void print_title(xmlNode *node) {
    xmlChar *title = xmlGetProp(node, (const xmlChar *) "title");
    printf("Tytuł strony: %s\n", title);
    xmlFree(title);
}

int main() {
    htmlDocPtr doc = htmlParseFile("index.html", NULL);
    
    xmlNode *root = xmlDocGetRootElement(doc);
    xmlNode *head = root->children->next;
    
    xmlNode *title = head->children;
    print_title(title);
    
    xmlFreeDoc(doc);
    return 0;
}
```

Rezultat wykonania tego programu dla strony internetowej "https://www.example.com/" będzie wyglądał następująco:

```
Tytuł strony: Example Domain
```

## Deep Dive

Biblioteka libxml2 zapewnia wiele funkcji do parsowania HTML, takich jak możliwość pobierania atrybutów elementów, szukania konkretnych elementów za pomocą wyrażeń XPath czy nawet tworzenia nowych dokumentów HTML. Warto przejrzeć jej dokumentację, aby wykorzystać wszystkie jej możliwości.

Istnieją również inne biblioteki, takie jak libtidy czy gumbo-parser, które mogą być użyte do parsowania HTML w C. Zawsze warto przetestować różne rozwiązania i wybrać to, które najlepiej pasuje do konkretnego zastosowania.

## Zobacz również

- Dokumentacja biblioteki libxml2: http://xmlsoft.org/html/index.html
- Przykładowe kody wykorzystujące inne biblioteki do parsowania HTML w C: https://github.com/gnesher/C-HTML-Parsers