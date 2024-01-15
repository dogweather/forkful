---
title:                "Eliminazione di caratteri corrispondenti a un pattern"
html_title:           "Clojure: Eliminazione di caratteri corrispondenti a un pattern"
simple_title:         "Eliminazione di caratteri corrispondenti a un pattern"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Se si lavora con testi e stringhe in Clojure, potrebbe essere necessario eliminare alcuni caratteri che corrispondono a un certo schema o modello. Ciò potrebbe essere utile per la pulizia dei dati o per ottenere una stringa formattata in un modo specifico.

## Come Fare

Per eliminare i caratteri che corrispondono a un determinato schema, è possibile utilizzare la funzione `clojure.string/replace` fornita dalla libreria standard di Clojure. Questa funzione accetta tre argomenti: una stringa di input, un pattern regex e una stringa di sostituzione. Ecco un esempio di come possiamo eliminare tutte le vocali da una stringa:

```Clojure
(clojure.string/replace "ciao come stai?" #"a|e|i|o|u" "")
```

Questo restituirà `"c cm st?"` come output. Possiamo anche fornire una funzione come stringa di sostituzione per eseguire un'operazione più complessa su ogni corrispondenza. Ad esempio, se vogliamo raddoppiare ogni consonante, possiamo fare qualcosa di simile a ciò:

```Clojure
(clojure.string/replace "hello world" #"[a-z]" (fn [m] (str m m)))
```

Questo restituirà `"hheelllloo  wwoorrlldd"` come output.

## Ricerca Profonda

Clojure offre molte funzioni di manipolazione delle stringhe tra cui scegliere e la maggior parte di esse consente di utilizzare espressioni regolari. Il pattern regex passato come secondo argomento della funzione `replace` può essere più complesso di una semplice stringa di corrispondenza, consentendoci di affinare la nostra ricerca e sostituzione in modo più preciso.

Inoltre, possiamo utilizzare la funzione `clojure.string/replace-first` per eliminare solo il primo match di una corrispondenza, anziché tutti. In alternativa, possiamo utilizzare la funzione `clojure.string/replace-regex` per sostituire i match solo quando corrispondono a un certo schema. Esplorare queste funzioni e scoprire come possono essere utili nelle operazioni di manipolazione delle stringhe in Clojure.

## Guarda Anche

- [Documentazione ufficiale di Clojure per la funzione `replace`](https://clojuredocs.org/clojure.string/replace)
- [Documentazione ufficiale di Clojure per le espressioni regolari](https://clojuredocs.org/clojure.core/re-pattern)
- [Esempi pratici di come utilizzare espressioni regolari in Clojure](https://www.masteringclojure.com/)