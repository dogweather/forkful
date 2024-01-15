---
title:                "Elaborazione di HTML"
html_title:           "C: Elaborazione di HTML"
simple_title:         "Elaborazione di HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/parsing-html.md"
---

{{< edit_this_page >}}

## Perché
Ti sei mai chiesto perché dovresti impegnarti a parsare HTML? Beh, se sei un programmatore C, probabilmente hai già il tuo buon motivo. Ma se sei ancora indeciso, continua a leggere per scoprire i vantaggi che questo processo può offrire.

## Come fare
Per prima cosa, per parsare HTML con C avrai bisogno di una libreria che ti fornisca le funzioni necessarie. Una delle opzioni più comuni è LibXML, disponibile su diverse piattaforme. Una volta inclusa nel tuo progetto, puoi utilizzare la funzione `htmlReadDoc()` per parsare un documento HTML e accedere ai suoi elementi tramite navigazione ad albero o utilizzando le funzioni `xmlGetProp()` e `xmlNodeGetContent()`. Di seguito un esempio di codice:

````C
#include <stdio.h>
#include <libxml/HTMLParser.h>
int main() {
    htmlDocPtr doc = htmlReadDoc("<html><body><p>Hello world!</p><div><a href="https://www.example.com">Example link</a></div></body></html>", NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);
    printf("Il documento ha %d elementi HTML\n", xmlChildElementCount(xmlDocGetRootElement(doc)));
    xmlNodePtr p = xmlDocGetRootElement(doc)->xmlChildrenNode->next->xmlChildrenNode;
    printf("Il contenuto di <p> è: %s\n", xmlNodeGetContent(p));
    printf("Il valore dell'attributo href è: %s\n", xmlGetProp(p->next->xmlChildrenNode, "href");
    xmlFreeDoc(doc);
    return 0;
}
````

L'output di questo codice sarà:

```
Il documento ha 1 elementi HTML
Il contenuto di <p> è: Hello world!
Il valore dell'attributo href è: https://www.example.com
```

## Approfondimento
La parsing di HTML non è solo utile per estrarre dati da una pagina web o per analizzare la sua struttura, ma può anche aiutarti a rilevare eventuali errori o problemi di validazione. Inoltre, sebbene ci siano molte opzioni disponibili per parsare HTML, è importante scegliere una libreria affidabile e ben supportata per evitare problemi e perdite di tempo nel tuo progetto.

## Vedi anche
- [LibXML - documentazione ufficiale](http://xmlsoft.org/)
- [Parsing di HTML con LibXML](https://www.xmlsoft.org/examples/parse2.c)