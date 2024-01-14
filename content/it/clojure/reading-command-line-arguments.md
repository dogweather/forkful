---
title:                "Clojure: Leggere gli argomenti della riga di comando"
simple_title:         "Leggere gli argomenti della riga di comando"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando in Clojure?

La lettura degli argomenti della riga di comando è fondamentale per l'interazione con il tuo programma Clojure da riga di comando. In questo articolo, impareremo come leggere e manipolare gli argomenti della riga di comando in modo semplice ed efficace.

## Come leggere gli argomenti della riga di comando

Per leggere gli argomenti della riga di comando in Clojure, utilizzeremo la funzione `command-line-args` che restituisce una lista di argomenti passati al programma nel momento dell'esecuzione. Vediamo un esempio di codice:

```Clojure
(defn sum-args [args]
  (reduce + (map #(Integer/parseInt %) args)))

(def args (command-line-args))

(println (format "La somma degli argomenti è: %d" (sum-args args)))
```

Input dalla riga di comando:

```
$ clojure -M -m my.program 1 2 3 4 5
```

Output:

```
La somma degli argomenti è: 15
```

In questo esempio, abbiamo definito una funzione `sum-args` che prende una lista di stringhe (gli argomenti) e li converte in numeri interi per calcolare la loro somma utilizzando la funzione `reduce` di Clojure.

## Approfondimento

Oltre alla funzione `command-line-args`, esistono altre librerie di terze parti che possono aiutarti a gestire gli argomenti della riga di comando in modo più avanzato, come ad esempio `tools.cli` e `clj-argparse`.

Inoltre, è importante notare che gli argomenti della riga di comando possono anche essere passati come opzioni, come nel seguente esempio:

```Clojure
(def opts
  [["-n" "--name NAME" "Il tuo nome"]])

(def settings
  (parse-opts *command-line-args* opts))

(println (format "Ciao %s!" (:name settings)))
```

Input dalla riga di comando:

```
$ clojure -M -m my.program -n "Mario"
```

Output:

```
Ciao Mario!
```

## Vedi anche

- [Clojure CLI Tools](https://clojure.org/guides/deps_and_cli) - Guida ufficiale alle CLI tools di Clojure.
- [tools.cli Library](https://github.com/clojure/tools.cli) - Libreria che semplifica la gestione degli argomenti della riga di comando.
- [clj-argparse Library](https://github.com/macourtney/clj-argparse) - Libreria che fornisce un parser di argomenti ispirato da argparse di Python.