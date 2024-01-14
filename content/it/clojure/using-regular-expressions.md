---
title:                "Clojure: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento importante per chiunque lavori con il codice e i dati. Sono utili per trovare, sostituire e manipolare testo in modo efficace e rapido.

## Come Fare

Le espressioni regolari in Clojure sono implementate attraverso il pacchetto `java.util.regex` e possono essere utilizzate in diversi contesti, come ad esempio la ricerca all'interno di stringhe, la validazione di input utente e la trasformazione di dati strutturati.

Per utilizzare le espressioni regolari in Clojure, è necessario prima importare il pacchetto utilizzando il seguente codice:

```Clojure
(require '[java.util.regex :as regex])
```

Una volta importato il pacchetto, è possibile utilizzare la funzione `re-matches` per cercare una corrispondenza tra una regex e una stringa. Ad esempio, per cercare una stringa che inizia con "ciao" e finisce con "mondo", si può utilizzare il seguente codice:

``` Clojure
(regex/re-matches #"ciao.*mondo" "ciao a tutti mondo")
```
Questo restituirebbe la corrispondenza trovata, ossia "ciao a tutti mondo".

## Approfondimento

Le espressioni regolari possono sembrare complicate a prima vista, ma sono uno strumento molto potente una volta imparate correttamente. Per comprendere meglio come utilizzarle, è importante familiarizzare con i diversi simboli e metacaratteri utilizzati nelle regex, come ad esempio `*`, `+` e `^`.

Inoltre, vi sono diversi siti e strumenti online che possono aiutare a testare e sperimentare con le espressioni regolari, come ad esempio RegExr e Regex101. Utilizzando queste risorse, è possibile approfondire la propria conoscenza delle regex e migliorare le propria capacità di utilizzarle in modo efficace.

## Vedere Anche

* [Documentazione ufficiale di Clojure](https://clojuredocs.org/clojure.core/re-matches)
* [Guida all'utilizzo delle espressioni regolari in Clojure](https://medium.com/codemonkey/clojure-regular-expressions-cheat-sheet-1bd4e8df09dc)
* [Sito per testare espressioni regolari](https://regexr.com/)