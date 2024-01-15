---
title:                "Scaricare una pagina web"
html_title:           "Clojure: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è un'operazione comune nella programmazione. Può essere utile per ottenere dati da un sito, analizzare informazioni o semplicemente mostrarlo all'utente.

## Come fare

Per scaricare una pagina web, abbiamo bisogno di utilizzare la libreria di Clojure chiamata `clj-http`. Quindi, prima di iniziare, assicurati di averla installata nel tuo progetto.

```Clojure
(require '[clj-http.client :as client])
```

Ora che abbiamo importato la libreria, possiamo procedere a scaricare una pagina web. Utilizziamo la funzione `client/get`, che richiede l'URL della pagina che vogliamo scaricare.

```Clojure
(client/get "https://clojure.org/")
```

Questo ci darà come output un oggetto con i dati della pagina web. Possiamo visualizzarlo stampando il corpo della risposta:

```Clojure
(client/get "https://clojure.org/")
(:body response)
```

## Approfondimento

La funzione `client/get` ci offre la possibilità di specificare ulteriori parametri per personalizzare la nostra richiesta. Ad esempio, possiamo specificare un header personalizzato utilizzando il parametro `:headers`, o aggiungere dei parametri alla query string utilizzando il parametro `:query-params`. 

Possiamo anche utilizzare `client/post` per fare una richiesta di tipo `POST` invece che `GET`, e specificare un corpo della richiesta utilizzando il parametro `:body`.

Per maggiori informazioni, puoi consultare la documentazione della libreria `clj-http` su [GitHub](https://github.com/dakrone/clj-http).

## Vedi anche

- [Documentazione di `clj-http`](https://github.com/dakrone/clj-http)
- [Esempi di utilizzo di `clj-http`](https://www.baeldung.com/http-client-clojure)