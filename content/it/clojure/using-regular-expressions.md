---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Bash: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Le espressioni regolari (regex) sono un potente strumento che permette ai programmatori di riconoscere, cercare e manipolare stringhe di testo basandosi su pattern specifici. Queste sono estremamente importanti per la manipolazione dei dati e la validazione dell'input dell'utente.

## Ecco Come:

Clojure fornisce diverse funzioni utili per lavorare con regex. Di seguito è illustrato un esempio di come utilizzare la funzione `re-find`:

```Clojure
(let [frase "Ciao, mi chiamo Mario"]
  (re-find #"mi chiamo (\w+)" frase))
;; => ["mi chiamo Mario" "Mario"]
```
In questo esempio, `re-find` cerca nella stringa `frase` l'espressione regolare `#"mi chiamo (\w+)"` e restituisce una coppia di risultati: l'intero match e il gruppo catturato.

## Approfondimenti

(1) Storicamente, le espressioni regolari sono nate negli anni '50 per teorizzare il concetto di "automi" e "linguaggi formali". Sono diventate uno strumento essenziale nella programmazione per la loro capacità di manipolare efficacemente le stringhe.

(2) Ci sono diverse alternative alle espressioni regolari. Ad esempio, i parser di stringhe o gli algoritmi di manipolazione di stringhe personalizzati. Tuttavia, le espressioni regolari sono spesso più efficienti e più facili da utilizzare per le operazioni di stringhe comuni.

(3) Clojure implementa le espressioni regolari con le Java regex. Quindi, quando si lavora con regex in Clojure, si sta realmente utilizzando le potenti funzionalità delle Java regex.

## Vedere Anche

- Per ulteriori informazioni sulle espressioni regolari in Clojure, consulta la [guida ufficiale](https://clojure.org/guides/learn/regular_expressions)
- Per esercizi pratici sulle regex in Clojure, visita [4Clojure](http://www.4clojure.com)
- Per maggiori dettagli sulla storia ed utilizzo delle espressioni regolari, vedi [Wikipedia](https://it.wikipedia.org/wiki/Espressione_regolare)