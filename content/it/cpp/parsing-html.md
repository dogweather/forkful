---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Il parsing HTML è il processo di estrazione dati da un documento HTML. I programmatori lo fanno per manipolare, interrogare o visualizzare questi dati in modi che vanno oltre il semplice rendering in un browser web.

## In pratica:

Qui di seguito è un esempio di come si può eseguire il parsing di un documento HTML in C++ utilizzando la libreria Gumbo. Prima di tutto, è necessario installare la libreria Gumbo.

```C++
#include <iostream>
#include <gumbo.h>

int main() {
    GumboOutput* output = gumbo_parse("<h1>Ciao, Mondo!</h1>");

    GumboNode* node = output->root;

    //Codice per lavorare con 'node' qui

    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```
Dopo l'esecuzione del codice, otterremo il nodo root del documento HTML, e da qui è possibile muoversi in tutto l'albero del documento.

## Approfondimento:

(1) Negli anni, ci sono stati vari approcci al parsing HTML, con diversi trade-off in termini di velocità, flessibilità, e conformità agli standard. La libreria Gumbo è una di queste soluzioni, sviluppata da Google, che punta a bilanciare tutte queste preoccupazioni.

(2) Altri metodi per il parse HTML includono l'uso di espressioni regolari (anche se questa tecnica è generalmente scoraggiata per la sua mancanza di flessibilità), SAX (Simple API for XML), e DOM (Document Object Model).

(3) Quando si esegue il parsing HTML con Gumbo, la libreria costruisce un albero di nodi che rappresenta il documento. Ogni nodo può essere un elemento, un testo, un commento, o uno spazio vuoto.

## Vedi Anche:

- Documentazione della libreria Gumbo: [Gumbo Parser](https://github.com/google/gumbo-parser)
  
- Guida di introduzione al parsing: [Introduction to Parsing](https://tomassetti.me/guide-parsing-algorithms-terminology)
  
- Articolo su perché non usare le espressioni regolari per il parsing HTML: [RegEx match open tags except XHTML self-contained tags](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)