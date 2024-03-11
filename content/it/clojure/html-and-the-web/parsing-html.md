---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:43.855184-07:00
description: "Il parsing di HTML in Clojure consiste nell'estrazione programmatica\
  \ di informazioni dai documenti HTML. I programmatori fanno ci\xF2 per accedere,\u2026"
lastmod: '2024-03-11T00:14:16.599276-06:00'
model: gpt-4-0125-preview
summary: "Il parsing di HTML in Clojure consiste nell'estrazione programmatica di\
  \ informazioni dai documenti HTML. I programmatori fanno ci\xF2 per accedere,\u2026"
title: Analisi del HTML
---

{{< edit_this_page >}}

## Cosa & Perché?

Il parsing di HTML in Clojure consiste nell'estrazione programmatica di informazioni dai documenti HTML. I programmatori fanno ciò per accedere, manipolare o monitorare dinamicamente i contenuti web, automatizzando compiti o alimentando applicazioni con dati.

## Come fare:

Clojure non dispone di capacità integrate di parsing HTML, ma puoi sfruttare le librerie Java o gli wrapper Clojure come `enlive` o `hickory`. Ecco come utilizzare entrambi:

### Usare Enlive:

Enlive è una scelta popolare per il parsing HTML e lo scraping web. Prima di tutto, includilo nelle dipendenze del tuo progetto:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Poi, puoi analizzare e navigare l'HTML così:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Questo frammento recupera una pagina HTML e seleziona tutti gli elementi `<div>` con la classe `some-class`.

L'output potrebbe assomigliare a:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Ecco del contenuto."]})
```

### Usare Hickory:

Hickory offre un modo per analizzare l'HTML in un formato che è più facile da gestire in Clojure. Aggiungi Hickory alle dipendenze del tuo progetto:

```clojure
[hickory "0.7.1"]
```

Ecco un esempio semplice:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Analizza l'HTML nel formato Hickory
(let [doc (hickory/parse "<html><body><div id='main'>Ciao, mondo!</div></body></html>")]
  ;; Seleziona il div con id 'main'
  (select/select (select/id "main") doc))
```

Questo codice analizza una semplice stringa HTML e utilizza un selettore CSS per trovare un `div` con l'ID `main`.

Esempio di output:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Ciao, mondo!"]}]
```

Sia `enlive` che `hickory` offrono soluzioni robuste per il parsing HTML in Clojure, con `enlive` che si concentra maggiormente sul templating e `hickory` che enfatizza la trasformazione dei dati.
