---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Leggere gli argomenti della riga di comando significa interpretare gli input che un utente fornisce al tuo programma quando lo lancia da una console o terminale. Lo facciamo perché questo apre la strada alla creazione di script o applicazioni più dinamici e interattivi.

## Come fare: Esempi di codice

Supponiamo di voler leggere gli argomenti della linea di comando in Clojure. Ecco un semplice esempio:

```Clojure
(defn -main
  [& args]
  (println "Hai fornito gli argomenti seguenti:" args))
```

E quando lo lanci fornendo dei parametri, vedrai qualcosa di simile nell'output:

```Shell
$ lein run arg1 arg2 arg3
Hai fornito gli argomenti seguenti: (arg1 arg2 arg3)
```

## Approfondiamo

Tradizionalmente, in linguaggi come C o Perl, la lettura degli argomenti della riga di comando avviene tramite l'array argv e l'intero argc. Clojure, essendo un linguaggio funzionale moderno, offre un'implementazione più pulita e intuitiva.

Un'alternativa consisterebbe nell’utilizzare librerie esterne, come tools.cli, che forniscono funzioni di parsing più elaborate.

Riguardo i dettagli di implementazione, `-main` è il punto di ingresso per i programmi basati su Clojure. Gli argomenti della riga di comando vengono passati come una lista alla funzione `-main`.

## Vedi Anche

1. Documentazione ufficiale di Clojure: [https://clojure.org/](https://clojure.org/)
2. La libreria tools.cli per un parsing avanzato degli argomenti della riga di comando: [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)
3. Articolo di Stack Overflow su Clojure e argomenti della riga di comando: [https://stackoverflow.com/questions/2352020/clojure-how-do-i-get-the-un-evaluated-command-line-arguments](https://stackoverflow.com/questions/2352020/clojure-how-do-i-get-the-un-evaluated-command-line-arguments)