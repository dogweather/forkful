---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:30:43.587967-07:00
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Il parsing di HTML consiste nell'analizzare il codice HTML per estrarne dati specifici. I programmatori lo fanno per automatizzare il recupero di informazioni da siti web e interagire con il web in modo programmatico.

## Come Fare:
Ecco un esempio di base che usa la libreria `enlive` per fare il parsing di un frammento HTML:

```clojure
(require '[net.cgrand.enlive-html :as enlive])

(def html-snippet "<html><body><p>Salve, mondo!</p></body></html>")

(defn parse-html [html]
  (enlive/select (enlive/html-resource (java.io.ByteArrayInputStream. (.getBytes html))) [:p]))

(println (parse-html html-snippet))
```

Output di esempio:

```
({:tag :p, :attrs nil, :content ["Salve, mondo!"]})
```

## Approfondimento:
Il parsing di HTML esiste da quando l'HTML è diventato il linguaggio standard per la creazione di pagine web. In Clojure, `enlive` è una libreria popolare per questa attività, ma ci sono alternative come `hickory` e `jsoup` (che richiede l'interoperabilità Java).

`enlive` utilizza un approccio basato su selettori simili a CSS per navigare e manipolare la struttura HTML. È potente per estrarre dati e può gestire anche documenti "maleformati" grazie alla sua tolleranza agli errori.

Se ti serve qualcosa di più orientato al web scraping, potresti considerare `clj-webdriver`, che è più adatto all'interazione diretta con il browser, ma è anche più pesante e complesso da usare.

## Vedere Anche:
- Enlive GitHub repository: [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)
- Documentazione Enlive: [https://github.com/cgrand/enlive/wiki](https://github.com/cgrand/enlive/wiki)
- Hickory: [https://github.com/davidsantiago/hickory](https://github.com/davidsantiago/hickory)
- Clj-webdriver: [https://github.com/semperos/clj-webdriver](https://github.com/semperos/clj-webdriver)
