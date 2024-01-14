---
title:                "Clojure: Lettura degli argomenti della riga di comando"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
Perché qualcuno dovrebbe leggere gli argomenti della riga di comando? Beh, spesso le applicazioni e i programmi hanno bisogno di ricevere informazioni dall'utente durante l'esecuzione, e l'utilizzo degli argomenti della riga di comando è un modo semplice e veloce per farlo.

## Come fare
Per leggere gli argomenti della riga di comando in Clojure, è necessario utilizzare la funzione "command-line-args". Questa funzione restituisce una lista con tutti gli argomenti passati al programma. Ecco un esempio di come utilizzarla:

```Clojure
(defn main [args]
  (println "Hai passato i seguenti argomenti: " args))
```

Se eseguiamo questo codice con il comando "lein run arg1 arg2", otterremo l'output:

```
Hai passato i seguenti argomenti: (arg1 arg2)
```

Come possiamo vedere, la funzione restituisce una lista di stringhe, dove ogni elemento corrisponde a un argomento passato. Possiamo quindi utilizzare funzioni come "first" o "nth" per accedere a un argomento specifico.

## Approfondimento
Ci sono alcune cose importanti da tenere a mente quando si leggono gli argomenti della riga di comando. Prima di tutto, è importante gestire le eccezioni che possono verificarsi se l'utente non passa gli argomenti corretti, ad esempio se il programma richiede un numero di argomenti specifico. Inoltre, bisogna prestare attenzione al tipo di argomenti che si riceve e eventualmente convertirli nel formato desiderato utilizzando funzioni come "Integer/parseInt" per convertire una stringa in un intero.

## Vedi anche
- https://clojuredocs.org/clojure.core/command-line-args
- https://clojure.org/guides/cli_arguments