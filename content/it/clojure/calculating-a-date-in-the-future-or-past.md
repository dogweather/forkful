---
title:    "Clojure: Calcolare una data nel futuro o nel passato"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per progetti che si basano su eventi temporali o per ottenere informazioni su date specifiche. Inoltre, può essere utile per automatizzare compiti o per creare applicazioni che tengono traccia di scadenze.

## Come eseguire il calcolo

Per iniziare a calcolare date in Clojure, è necessario utilizzare la libreria datetime, che contiene funzioni utili per la gestione del tempo. Iniziamo importando questa libreria nel nostro codice:

```Clojure
(ns date-calc
  (:require [clojure.java-time :as t]))
```

Per calcolare una data nel futuro, utilizziamo la funzione ```t/plus```, specificando il numero di anni, mesi, giorni, ore e minuti che vogliamo aggiungere alla data attuale. Ad esempio, se vogliamo ottenere la data di oggi più 1 mese e 5 giorni, il codice sarà il seguente:

```Clojure
(t/plus (t/today) {:months 1 :days 5})
```

Il risultato sarà una data nel futuro. Per calcolare una data nel passato, possiamo utilizzare la funzione ```t/minus``` con lo stesso approccio.

## Approfondimento

Clojure offre diverse funzioni utili per la gestione del tempo, come ```t/with-zone``` per impostare un fuso orario specifico, ```t/days-in-month``` per ottenere il numero di giorni in un determinato mese e ```t/week-day``` per ottenere il giorno della settimana di una data specifica.

Inoltre, è possibile utilizzare le funzioni di confronto delle date, come ```t/before?``` e ```t/after?```, per verificare se una data si trova prima o dopo un'altra.

Per maggiori informazioni su queste funzioni e per scoprire altre utili funzioni di datetime, si consiglia di consultare la documentazione ufficiale di Clojure.

## Vedi anche

- Documentazione ufficiale di Clojure: https://clojure.org/
- Documentazione della libreria datetime: https://cljdoc.org/d/clojure.java-time/clojure.java-time/0.3.2/doc/javadoc/index
- Esempi pratici di utilizzo di datetime in Clojure: https://gist.github.com/rkneufeld/11e5164c1125451ab922