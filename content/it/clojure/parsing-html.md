---
title:                "Clojure: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Perché
C'è una grande quantità di informazioni disponibili online, ma spesso è difficile estrarre i dati di cui abbiamo bisogno. Il parsing HTML ci permette di analizzare il codice sorgente di una pagina web e recuperare le informazioni importanti in un formato strutturato.

## Come fare
Per iniziare a fare il parsing HTML in Clojure, dobbiamo installare la libreria 'clj-tagsoup' che ci permetterà di lavorare con il codice HTML come se fosse un albero di dati:

```Clojure
(ns html-parser.core
  (:require [clojure.xml :as xml]
            [tagsoup.core :refer [parse-xml]]))

(def html (slurp "http://www.example.com"))

(def tree (parse-xml html))

;; Per esempio, possiamo recuperare il titolo della pagina:
(get-in tree [:title 0 :content])
;; Output: "Esempio.com - Il tuo sito web di esempio"
```

Con la funzione 'slurp' stiamo prendendo il codice HTML della pagina web e passandolo alla funzione 'parse-xml' che lo converte in una struttura dati che può essere facilmente manipolata con le funzioni di Clojure come 'get-in'.

## Approfondimento
Oltre al semplice parsing dei dati, è possibile combinare il parsing HTML con altre funzionalità di Clojure per eseguire operazioni più complesse. Ad esempio, possiamo utilizzare la libreria 'clj-http' per effettuare richieste HTTP e recuperare i dati da più pagine web.

Inoltre, è possibile accedere ai singoli elementi HTML utilizzando le specifiche CSS o XPath, grazie alle funzioni fornite dalle librerie 'hiccup' e 'enlive'.

## Vedi anche
- [Documentazione della libreria 'clj-tagsoup'](https://github.com/nathell/clj-tagsoup)
- [Esempio di scraping web con Clojure](https://medium.com/coding-with-clarity/web-scraping-in-clojure-510f928915e0)
- [Documentazione della libreria 'clj-http'](https://github.com/dakrone/clj-http)
- [Tutorial su come utilizzare il parsing HTML con Clojure](https://www.javaworld.com/article/3188004/web-development/html-parsing-in-clojure.html)