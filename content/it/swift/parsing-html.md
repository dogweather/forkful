---
title:                "Estrazione di html"
html_title:           "Swift: Estrazione di html"
simple_title:         "Estrazione di html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Che cos'è e perché si fa

Il parsing HTML è il processo di analisi di codice HTML per estrarre informazioni utili dai siti web. I programmatori lo fanno per creare strumenti in grado di elaborare e manipolare i dati HTML in modo più preciso ed efficiente.

## Come fare

Ecco un semplice esempio di come analizzare una pagina HTML utilizzando Swift:

```Swift
// importa il framework HTMLString
import HTMLString 

// definisci una funzione che esegue il parsing della pagina HTML
func parseHTML() {
    // scarica la pagina HTML desiderata
    guard let url = URL(string: "https://www.example.com") else {
        print("Impossibile scaricare la pagina")
        return
    }
    do {
        // utilizza il metodo .contents() per estrarre il contenuto della pagina
        let htmlText = try String(contentsOf: url)
        // crea un oggetto HTMLDocument utilizzando il contenuto della pagina
        let htmlDocument = try HTMLDocument(string: htmlText)
        
        // utilizza il metodo .forEachNode() per scorrere tutti i nodi del documento HTML
        htmlDocument.forEachNode { node in
            // se il nodo ha un elemento "a", stampa il suo valore attributo href
            if let a = node as? HTMLHyperLink {
                print(a.href)
            }
        }
    } catch {
        print("Errore durante il parsing HTML")
    }
}
```

Esempio di output: ```https://www.example.com/links```

## Approfondimento

Il parsing HTML è stato introdotto per la prima volta nel 1986 nell'ambito del progetto SGML (Standard Generalized Markup Language). Una delle alternative più diffuse al parsing HTML è l'utilizzo di librerie come BeautifulSoup o libxml2.

Per implementare il parsing HTML, Swift utilizza il framework HTMLString, che permette di manipolare e analizzare facilmente il codice HTML. Esso fornisce anche una serie di metodi utili per gestire i dati estratti dai siti web.

## Vedi anche

Per ulteriori informazioni sul parsing HTML in Swift, puoi consultare la documentazione ufficiale del framework HTMLString o il repository GitHub relativo. Inoltre, puoi trovare ulteriori esempi ed esercizi su siti come Ray Wenderlich o Hacking with Swift.