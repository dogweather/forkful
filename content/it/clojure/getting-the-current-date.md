---
title:    "Clojure: Ottenerre la data attuale"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui qualcuno potrebbe voler ottenere la data corrente in un programma Clojure. Ad esempio, potrebbe essere utile per tenere traccia del tempo di esecuzione di un'operazione o per mostrare la data di creazione di un file.

## Come fare

Per ottenere la data corrente in Clojure, puoi utilizzare la funzione "now" del modulo "java.time". Prima di tutto, importa il modulo con il comando ```Clojure (require '[java.time :as time])```. Poi, utilizza la funzione con il formato desiderato, ad esempio ```Clojure (time/now)``` per ottenere la data e l'ora correnti.

Per ottenere solo la data in formato stringa, puoi utilizzare la funzione "format" insieme alla costante "ISO_DATE" come formato, ad esempio ```Clojure (time/format (time/now) time/ISO_DATE)```.

## Approfondimento

Per ulteriori informazioni sulla gestione delle date in Clojure, puoi consultare la documentazione del modulo "java.time" e del linguaggio Clojure. Inoltre, puoi esplorare ulteriormente le funzioni disponibili per formattare e manipolare le date, come ad esempio "plus-days" e "minus-years" per aggiungere o sottrarre una determinata quantità di giorni o anni.

## Vedi Anche

- Documentazione del modulo "java.time": https://clojure.org/guides/java_interop#_using_joda_time
- Documentazione del linguaggio Clojure: https://clojure.org/api/cheatsheet
- Manipolazione delle date in Clojure: https://clojuredocs.org/clojure.instant/days