---
title:                "Leggere gli argomenti dalla riga di comando"
html_title:           "Clojure: Leggere gli argomenti dalla riga di comando"
simple_title:         "Leggere gli argomenti dalla riga di comando"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere gli argomenti dalla riga di comando è una pratica comune che i programmatori utilizzano per interagire con un programma durante l'esecuzione. Questi argomenti possono essere forniti dall'utente e influenzare il comportamento del programma.

## Come:

```Clojure
(defn -main
  [& args]
  (println "Benvenuto! Gli argomenti che hai passato sono:")
  (println args))
  
```
Esclusivo alla funzione ```-main``` di Clojure, la variabile ```args``` conterrà una lista di tutti gli argomenti passati dalla riga di comando. Possiamo quindi stamparli a schermo utilizzando la funzione ```println```. Ad esempio, se eseguiamo il programma con gli argomenti "programma Clojure", otterremo l'output:

```
Benvenuto! Gli argomenti che hai passato sono:
("programma" "Clojure")
```

## Approfondimento:

L'utilizzo degli argomenti dalla riga di comando è stato introdotto negli anni '70 con la creazione dei primi sistemi operativi a linea di comando. Oggi è ancora una pratica comune, ma ci sono alternative come l'utilizzo di variabili d'ambiente o la lettura da un file di configurazione.

Per implementare la lettura degli argomenti dalla riga di comando, Clojure utilizza la funzione ```& args``` che combina automaticamente tutti gli argomenti passati in una lista.

## Vedi anche:

- [Documentazione ufficiale di Clojure sulla lettura degli argomenti dalla riga di comando](https://clojure.org/reference/evaluation#_command_line_args)