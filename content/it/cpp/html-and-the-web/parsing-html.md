---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:35.652552-07:00
description: "Come fare: C++ non viene fornito con capacit\xE0 di analisi HTML integrate.\
  \ Spesso si utilizza una libreria come Gumbo-parser di Google o qualcosa di simile.\u2026"
lastmod: '2024-03-13T22:44:43.724404-06:00'
model: gpt-4-0125-preview
summary: "C++ non viene fornito con capacit\xE0 di analisi HTML integrate."
title: Analisi del HTML
weight: 43
---

## Come fare:
C++ non viene fornito con capacità di analisi HTML integrate. Spesso si utilizza una libreria come Gumbo-parser di Google o qualcosa di simile. Ecco un rapido esempio utilizzando Gumbo-parser:

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

Esempio di output:
```
https://example.com
```

## Approfondimento
Analizzare HTML non è sempre stato semplice in C++. Storicamente, i programmatori utilizzavano espressioni regolari o parser scritti a mano, entrambi fonti di errori e difficoltosi. Oggi, librerie robuste come Gumbo-parser gestiscono le complessità dell'analisi, rendendola più semplice ed affidabile.

Le alternative includono Tidy, MyHTML, o anche integrare C++ con BeautifulSoup di Python tramite la funzione `system` di C++ o interpreti integrati.

Dal punto di vista dell'implementazione, queste librerie convertono HTML in un albero Modello Oggetto Documento (DOM). Attraversare e manipolare il DOM consente agli utenti di estrarre e lavorare con i dati come dimostrato nella sezione Come fare.

## Vedere Anche
- [Repository GitHub di Gumbo-parser](https://github.com/google/gumbo-parser)
- [Lista di librerie per l'analisi HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Interoperabilità tra C++ e Python](https://docs.python.org/3/extending/embedding.html)
