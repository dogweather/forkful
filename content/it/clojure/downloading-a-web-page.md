---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

---
title: Gestire il Web con Clojure: Scaricare pagine web
---

## Cos'è & Perché?

Scaricare una pagina web è il processo di ottenere i dati di quella pagina dal server sul quale è ospitata. Questo è utile per gli sviluppatori per analizzare o manipolare i dati, oppure per creare servizi come motori di ricerca o aggregatori di notizie.

## Come fare

Avrai bisogno della libreria `clj-http` per raggiungere il tuo obiettivo. Aggiungila al tuo progetto Leiningen aggiungendo la seguente riga alle tue dipendenze:

```Clojure
[clj-http "3.12.3"]
```

Ecco come scaricare una pagina web:

```Clojure
(defn scaricare-pagina [url]
  (let [risposta (clj-http.client/get url)]
    (:body risposta)))

(println (scaricare-pagina "https://www.google.it"))
```

Questo programma invia una richiesta GET all'URL specificato e stampa il corpo della risposta, cioè i contenuti della pagina web.

## Approfondimento

Normalmente, utilizzare la libreria `clj-http` è il modo più semplice per scaricare una pagina web in Clojure. Tuttavia, in passato, i programmatori dovevano utilizzare funzioni di più basso livello fornite da librerie come `java.net.URL`.

Esistono alternative a `clj-http`, come `http-kit` o `aleph`, che possono fornire prestazioni migliori in certe situazioni, ma spesso a costo di una maggiore complessità.

Dettagli di implementazione interessanti includono il fatto che `clj-http` utilizza il pool di connessioni di Apache HttpClient, che consente di riutilizzare le connessioni HTTP per efficienza. Inoltre, supporta una vasta gamma di funzionalità, come l'autenticazione, i cookie, l'interazione con API RESTful, e altro ancora.

## Vedi Anche

1. Documentazione di [clj-http](https://github.com/dakrone/clj-http)
2. [http-kit](http://www.http-kit.org/)
3. [aleph](https://github.com/ztellman/aleph)