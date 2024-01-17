---
title:                "Analisi dell'html"
html_title:           "C: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/parsing-html.md"
---

{{< edit_this_page >}}

## Che cos'è e perché lo facciamo?

Il parse di HTML è la pratica di analizzare il codice HTML di una pagina web per estrarre informazioni utili, come il testo, le immagini, i link e le etichette di formattazione. Lo facciamo per automatizzare il processo di raccolta di dati e per analizzare e manipolare il contenuto delle pagine web in modo efficiente.

## Come fare:

Per iniziare a fare il parse di HTML in C, dovrai utilizzare una libreria appositamente progettata per questo scopo. Una delle più popolari è libxml2, che puoi installare con il tuo gestore di pacchetti preferito. Ecco un esempio di codice che utilizza libxml2 per fare il parse di una pagina web e estrarre il suo titolo:

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {

    // Creare il puntatore al documento
    htmlDocPtr html_doc;
    
    // Caricare il documento HTML
    html_doc = htmlReadFile("pagina_web.html", NULL, 0);
    
    // Ottenere l'elemento radice del documento
    xmlNodePtr root_node = xmlDocGetRootElement(html_doc);
    
    // Ottenere il primo figlio dell'elemento radice (che dovrebbe essere l'etichetta <head>)
    xmlNodePtr head = root_node->xmlChildrenNode;
    
    // Ottenere l'elemento <title> contenuto in <head>
    xmlNodePtr title = head->xmlChildrenNode;
    
    // Stampare il contenuto di <title>
    printf("%s\n", title->content);
    
    // Rilasciare la memoria utilizzata
    xmlFreeDoc(html_doc);
    
    return 0;
}
```

Ecco un esempio di output per una pagina web con il seguente codice HTML:

```html
<html>
<head>
    <title>Il mio sito web</title>
</head>
<body>
    <h1>Benvenuti nel mio sito!</h1>
    <p>Questo è un paragrafo di testo.</p>
    <a href="https://www.example.com">Link al mio sito</a>
</body>
</html>
```

```
Il mio sito web
```

## Approfondimenti:

Il parsing di HTML è stato uno dei primi metodi per ottenere informazioni da una pagina web. In passato, si usava spesso il parsing manuale, ossia l'analisi del codice HTML tramite uno script o un programma per estrarre le informazioni desiderate. Oggi, invece, ci sono molte librerie di parsing, come libxml2 e Gumbo, che semplificano notevolmente il processo.

Un'altra alternativa al parsing di HTML è l'utilizzo di API che forniscono i dati tramite richieste HTTP. Tuttavia, questo metodo può essere più complesso, soprattutto per siti web più grandi e complessi.

Per quanto riguarda gli implementation details, libxml2 utilizza una libreria di parsing XML per leggere il codice HTML e creare una rappresentazione ad albero del documento. Questa rappresentazione è molto utile per navigare e manipolare il documento in modo più facile. Inoltre, libxml2 gestisce anche gli errori di parsing e di codifica, rendendolo una scelta affidabile per il parsing di HTML in C.

## Vedi anche:

- [Libxml2 documentazione ufficiale](http://www.xmlsoft.org/html/libxml-parser.html)
- [Gumbo: Una libreria di parsing HTML5 in C](https://github.com/google/gumbo-parser)
- [Introduzione al parsing di HTML](https://www.w3.org/2004/04/htmltidy/Tidy.html)