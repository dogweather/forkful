---
title:    "Clojure: Lettura degli argomenti della riga di comando"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Leggere gli argomenti dalla riga di comando è un'abilità importante per ogni programmatore Clojure. Consente di rendere i tuoi programmi più flessibili e personalizzabili per gli utenti, che possono passare informazioni diverse ogni volta che eseguono il tuo codice. In questo articolo, vedremo come leggere e gestire gli argomenti dalla riga di comando utilizzando Clojure.

## Come fare

Per leggere gli argomenti dalla riga di comando in Clojure, utilizzeremo la funzione `clojure.core/command-line-args`. Questa funzione prende gli argomenti dalla riga di comando e li restituisce come una lista di stringhe.

```Clojure
(def args (command-line-args))

(print "I parametri dalla riga di comando sono" args)
```

Ecco un esempio di invocazione del programma con argomenti:

```
$ java -jar mio_programma.jar arg1 arg2
I parametri dalla riga di comando sono ["arg1" "arg2"]
```

Come puoi vedere dall'esempio, gli argomenti vengono passati al programma dopo il nome del file .jar. È possibile specificare qualsiasi numero di argomenti e verranno tutti letti dalla funzione `command-line-args`.

## Approfondimento

Oltre alla funzione `command-line-args`, è possibile utilizzare anche la libreria `tools.cli` per leggere e gestire gli argomenti dalla riga di comando in modo più strutturato. Questa libreria consente di definire opzioni con parametri e fornire un'interfaccia più intuitiva per l'utente. Puoi consultare la documentazione ufficiale di `tools.cli` per saperne di più su come utilizzarla.

## Vedi anche

- [Documentazione ufficiale di command-line-args](https://clojuredocs.org/clojure.core/command-line-args)
- [Documentazione ufficiale di tools.cli](https://clojure.github.io/tools.cli/)