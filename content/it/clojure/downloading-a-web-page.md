---
title:                "Scaricare una pagina web."
html_title:           "Clojure: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il download di una pagina web è semplicemente il processo di ottenere il contenuto di una pagina web da Internet. I programmatori spesso fanno questo per accedere a dati importanti o per automatizzare alcune attività.

## Come fare:
Per effettuare il download di una pagina web in Clojure, è possibile utilizzare la libreria `clj-http`. Inizialmente, è necessario importare questa libreria all'interno del proprio codice usando la seguente direttiva:

```Clojure
(ns mia-app.core
  (:require [clj-http.client :as client]))

```
A questo punto, è possibile utilizzare la funzione `client/get` per inviare una richiesta GET a una pagina web e ottenere il suo contenuto come stringa:

```Clojure
(def pagina (client/get "https://www.example.com"))
```

Il risultato sarà salvato nella variabile `pagina`. 

## Approfondimento:
Il download di una pagina web è diventato una parte essenziale della programmazione moderna, poiché la maggior parte delle applicazioni web si basano su dati ottenuti dal web. Inoltre, esistono anche altre alternative per effettuare il download di una pagina web in Clojure, come ad esempio utilizzando la libreria `clojure.java.io` o utilizzando la JVM diretta.

È importante notare che il download di una pagina web può anche essere utilizzato per fini malevoli, quindi è sempre importante fare attenzione a quale pagina viene scaricata e come viene utilizzata.

Per ulteriori informazioni sulla libreria `clj-http`, è possibile consultare la sua documentazione ufficiale su GitHub: https://github.com/dakrone/clj-http

## Vedi anche:
- Guida completa su come utilizzare la libreria `clj-http`: https://github.com/dakrone/clj-http/wiki
- Altre opzioni per effettuare il download di una pagina web in Clojure: https://medium.com/@draganrocksaca/web-scraping-in-clojure-c50fe40c0b9c