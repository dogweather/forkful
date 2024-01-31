---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:30:21.039794-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Il parsing di HTML significa leggere e interpretare il codice HTML per estrarne dati o struture. I programmatori lo fanno per automatizzare l'interazione con le pagine web, estrarre informazioni, o alimentare servizi web scraping.

## How to: (Come fare:)
In C++, possiamo usare la libreria `libxml2` per fare parsing dell'HTML. Qui un esempio di base:

```C++
#include <iostream>
#include <libxml/HTMLparser.h>

int main() {
    const char* htmlContent = "<html><body><p>Hello, World!</p></body></html>";
    htmlDocPtr doc = htmlReadDoc((xmlChar*)htmlContent, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);

    // Assumiamo che il parsing non fallisca.
    xmlNode *root_element = xmlDocGetRootElement(doc);

    // Stampa il contenuto del tag <p>.
    for (xmlNode *currentNode = root_element; currentNode; currentNode = currentNode->next) {
        if (currentNode->type == XML_ELEMENT_NODE && strcmp((const char *)currentNode->name, "body") == 0) {
            xmlNode *pNode = currentNode->children->next;
            if (pNode) {
                std::cout << pNode->name << ":" << pNode->children->content << std::endl;
            }
        }
    }
    
    // Pulizia.
    xmlFreeDoc(doc);
    xmlCleanupParser();

    return 0;
}
```

Output:
```
p:Hello, World!
```

## Deep Dive (Approfondimenti)
### Contesto storico
Il parsing di HTML è antico quanto il web stesso, ma le librerie moderne hanno semplificato molto il processo.

### Alternative
Esistono diverse librerie per il parsing HTML in C++ come `Gumbo` o `QtWebKit`. I programmatori scelgono in base alle proprie necessità.

### Dettagli implementativi
`libxml2` implementa il DOM e SAX per processi di parsing. Può gestire anche XML difettosi grazie all'opzione `HTML_PARSE_RECOVER`.

## See Also (Vedi anche)
- Documentazione di `libxml2`: http://xmlsoft.org/html/libxml-HTMLparser.html
- Progetto `Gumbo`: https://github.com/google/gumbo-parser
- QtWebKit: https://doc.qt.io/qt-5/qtwebkit-guide.html
- Informazioni su web scraping con C++: https://www.scrapingbee.com/blog/web-scraping-c/
