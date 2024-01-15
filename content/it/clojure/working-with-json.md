---
title:                "Lavorare con json"
html_title:           "Clojure: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con dati strutturati, come ad esempio quelli provenienti da API web, molto probabilmente ti imbatterai nel formato JSON. Clojure offre diverse librerie e funzioni per lavorare con questo formato dati in modo semplice e intuitivo.

## Come fare

```Clojure
(def articolo {:titolo "Come utilizzare Clojure e JSON"
               :data "3 settembre 2021"
               :scrittore "John Smith"
               :tags ["programmazione" "Clojure" "JSON"]})

(require '[clojure.data.json :as json])

;; Convertire un dato Clojure in JSON
(json/write-value articolo) ;; Ritorna una stringa rappresentante il JSON

;; Convertire un JSON in un dato Clojure
(json/read-str "{\"nome\": \"Maria\", \"cognome\": \"Rossi\"}") ;; Ritorna una mappa Clojure

;; Caricare un file JSON
(with-open [reader (clojure.java.io/reader "articoli.json")]
  (json/read reader))

;; Scaricare un JSON da una URL
(json/read (clojure.java.io/reader "https://www.example.com/api"))

```

### Esempio di output

Stringa in formato JSON:

```Clojure
"{\"titolo\": \"Come utilizzare Clojure e JSON\", \"data\": \"3 settembre 2021\", \"scrittore\": \"John Smith\", \"tags\": [\"programmazione\", \"Clojure\", \"JSON\"]}"
```

Mappa Clojure:

```Clojure
{:nome "Maria", :cognome "Rossi"}
```

JSON caricato da file o URL:

```Clojure
[{:titolo "Articolo 1", :testo "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", :autore "John Smith"}, {:titolo "Articolo 2", :testo "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.", :autore "Jane Doe"}]
```

## Approfondimento

Clojure offre la possibilità di lavorare con JSON utilizzando la libreria `clojure.data.json` o la funzione `clojure.edn/read`. Entrambe consentono di convertire dati Clojure in JSON e viceversa. È importante notare che JSON non supporta la rappresentazione di tutte le strutture dati di Clojure, come ad esempio le funzioni. Inoltre, è necessario prestare attenzione alla gestione delle chiavi duplicate poiché JSON non permette la presenza di più chiavi con lo stesso nome in un oggetto.

## Vedi anche

- [Documentazione ufficiale di Clojure sul lavoro con JSON](https://clojure.org/reference/data_structures#_json)
- [Articolo di Medium su come utilizzare Clojure con JSON](https://medium.com/@conan}arvelo/clojure-and-json-fe7ef66094d0)
- [Esempi pratici di utilizzo di JSON con Clojure](https://gist.github.com/dwalisz89/1071265ed513c62deeaeedd39a7c4ab2)