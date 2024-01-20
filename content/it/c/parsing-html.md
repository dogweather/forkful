---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
L'analisi o parsing dell'HTML è il processo di lettura e comprensione del codice HTML al fine di creare una rappresentazione interna, detta albero DOM. I programmatori lo fanno per manipolare, analizzare e ristrutturare le pagine web.

## Come fare:
Il seguente codice serve come esempio basilare di analisi HTML. Utilizza la libreria di parsing Gumbo, sviluppata e manutenuta da Google.

```C
#include <stdio.h>
#include <gumbo.h>

static void cerca_nodi(GumboNode* nodo) {
    if (nodo->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboVector* nodi_figli = &nodo->v.element.children;
    for (unsigned int i = 0; i < nodi_figli->length; ++i) {
        cerca_nodi(nodi_figli->data[i]);
    }
}

int main() {
    GumboOutput* output = gumbo_parse("<h1>Ciao, Mondo!</h1>");
    cerca_nodi(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```
Quando eseguito, questo programma non restituirà nulla, ma analizzerà l'HTML fornito e creerà un albero DOM corrispondente.

## Approfondimenti
(1) L'HTML, come linguaggio di markup, esiste dal 1991 e nel corso degli anni i metodi di parsing si sono evoluti notevolmente. Dall'analisi regolare delle espressioni all'introduzione delle API di parsing come Gumbo da Google.
(2) Esistono diverse alternative alla libreria Gumbo, tra cui html5lib e libxml2, ognuna con i suoi pro e contro.
(3) Nota importante riguardo all'implementazione: la memoria allocata da gumbo_parse deve essere distrutta con gumbo_destroy_output per evitare perdite di memoria.

## Vedi Anche
- Documentazione della libreria Gumbo: https://github.com/google/gumbo-parser
- Documentazione della libreria html5lib: https://github.com/html5lib/html5lib-python
- Documentazione della libreria libxml2: http://xmlsoft.org/
- Una guida all'albero DOM: https://developer.mozilla.org/it/docs/Web/API/Document_Object_Model