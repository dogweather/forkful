---
title:                "Clojure: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è una delle attività fondamentali quando si lavora con dati online. Può essere utile per analizzare contenuti, ottenere informazioni o per semplicemente archiviare una pagina per la lettura offline.

## Come Fare

Il primo passo per scaricare una pagina web in Clojure è utilizzare la libreria `clj-http`. Assicurati di aggiungere questa libreria alla tua dipendenza nel file `project.clj`:

```Clojure
:dependencies [[clj-http "3.11.0"]]
```

Dopo aver aggiornato le dipendenze del progetto, è possibile utilizzare la funzione `client/get` per scaricare una pagina web. Ad esempio, per scaricare la homepage di Google possiamo usare questo codice:

```Clojure
(require '[clj-http.client :as client])

(defn get-google []
  (let [response (client/get "https://www.google.com")]
    (:body response)))

(get-google)
```

Proviamo ad eseguire il codice sopra in una REPL, noteremo che il risultato sarà una stringa contenente il contenuto della pagina HTML di Google.

```
"<html>\n<head>\n<meta http-equiv='content-type' content='text/html;charset=utf-8'>\n ...
```

Oltre a `:body`, la funzione `client/get` restituisce anche altri dati utili come `:headers` e `:status` che possono essere utilizzati per analizzare o gestire la risposta della richiesta.

## Approfondimento

Ora che abbiamo una prima idea di come scaricare una pagina web in Clojure, possiamo esplorare alcune funzionalità avanzate della libreria `clj-http`.

Per esempio, è possibile specificare parametri nella richiesta GET utilizzando il parametro `:query-params`:

```Clojure
(client/get "https://www.google.com/search"
            :query-params {:q "clojure"})
```

In questo caso, stiamo scaricando la pagina dei risultati di Google per la ricerca "clojure". Possiamo anche specificare header nella richiesta utilizzando il parametro `:headers` e impostare un timeout utilizzando il parametro `:timeout`.

Inoltre, la libreria `clj-http` supporta anche richieste POST, PUT, DELETE e altre.

## Vedi Anche

- Documentazione ufficiale di `clj-http`: https://github.com/dakrone/clj-http
- Tutorial su scraping di pagine web in Clojure: https://clojureverse.org/t/clojure-web-scraping-tutorial/1454