---
date: 2024-01-20 17:43:40.392592-07:00
description: 'How to: Usiamo `clj-http` per scaricare una pagina web. Si tratta di
  una biblioteca Clojure semplice ma potente.'
lastmod: '2024-03-13T22:44:43.041138-06:00'
model: gpt-4-1106-preview
summary: Usiamo `clj-http` per scaricare una pagina web.
title: Scaricare una pagina web
weight: 42
---

## How to:
Usiamo `clj-http` per scaricare una pagina web. Si tratta di una biblioteca Clojure semplice ma potente.

```Clojure
(require '[clj-http.client :as client])

(defn download-web-page [url]
  (client/get url))

;; Usiamo la funzione e stampiamo il corpo della risposta:
(println (:body (download-web-page "http://example.com")))
```

Esempio di output:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

La funzione `download-web-page` fa una chiamata GET all'URL specificato e restituisce il contenuto.

## Deep Dive
`clj-http` sfrutta la libreria Apache HttpComponents per offrire un'interfaccia Clojure per le richieste HTTP. Storico ma affidabile, è ampiamente utilizzato nella comunità Clojure. Come alternativa, alcuni preferiscono `http-kit` per le sue funzionalità asincrone o `Aleph` per i protocolli basso-livello. Importante: le pagine web possono avere termini d'uso che limitano lo scraping; da considerare nella scelta dell'approccio di download.

## See Also
Per approfondire:

- [clj-http su GitHub](https://github.com/dakrone/clj-http)
- [Documentazione di Clojure](https://clojure.org/guides/getting_started)
- [Una guida a http-kit](http://www.http-kit.org/)
- [Aleph per la programmazione reattiva](https://github.com/aleph-io/aleph)
