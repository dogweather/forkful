---
title:                "Parsing html"
html_title:           "C++: Parsing html"
simple_title:         "Parsing html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il parsing HTML è il processo di analisi dei contenuti di una pagina web e della loro struttura per estrarre informazioni significative. I programmatori si impegnano nello sviluppo di algoritmi di parsing HTML per creare applicazioni che possano elaborare e manipolare i dati dei siti web in modo efficiente.

## Come fare:
 Di seguito sono riportati alcuni esempi di codice in ```C++``` che illustrano come eseguire il parsing HTML utilizzando la libreria ```libxml2```. Il codice di seguito mostra come ottenere il contenuto di un elemento ```<p>```:

```
#include <libxml/HTMLparser.h>

void parseHTML(char *filename) {
    htmlDocPtr doc; /* puntatore al documento HTML da analizzare*/
    xmlNodePtr cur; /* puntatore al nodo corrente*/

    doc = htmlReadFile(filename, NULL, HTML_PARSE_NOBLANKS | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING | HTML_PARSE_NONET);
    cur = xmlDocGetRootElement(doc);

    if (cur == NULL) {
        fprintf(stderr,"Errore nella lettura del documento\n");
        return;
    }

    cur = cur->children; // si passa al primo nodo figlio
    while (cur != NULL && strcmp((const char*)cur->name, "p"))
        cur = cur->next; // si scorrono i nodi figli fino a trovare quello con l'elemento "p"

    printf("Il contenuto dell'elemento <p> è %s\n", xmlNodeGetContent(cur)); // si stampa il contenuto del tag <p>
    
    xmlFreeDoc(doc); // si rilasciano le risorse allocate
    return;
}
```

## Approfondimento:
Il parsing HTML ha una lunga storia, a partire dal suo utilizzo pionieristico negli anni '90 per la creazione dei primi motori di ricerca. Oggi, esistono varie alternative per eseguire il parsing HTML, tra cui librerie come ```libxml2```, ```HTMLParser``` e ```BeautifulSoup```. Il processo di parsing può essere complesso, in quanto è necessario gestire varie eccezioni e gli standard del linguaggio web sono in costante evoluzione.

## Vedi anche:
- [Documentazione ufficiale di libxml](http://xmlsoft.org/html/)
- [Documentazione ufficiale di HTMLParser](https://htmlparser.sourceforge.io/)
- [Documentazione ufficiale di BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)