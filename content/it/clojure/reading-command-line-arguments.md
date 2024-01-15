---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Clojure: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

L'utilizzo degli argomenti della riga di comando è essenziale per rendere i nostri programmi più dinamici e versatili. Ci permettono di passare informazioni e opzioni al nostro codice senza doverlo modificare ogni volta che lo eseguiamo.

## Come fare

Per leggere gli argomenti della riga di comando in Clojure, possiamo utilizzare la funzione *command-line-args* che restituisce una lista contenente gli argomenti forniti al momento dell'esecuzione del programma. Possiamo quindi utilizzare la funzione *nth* per accedere agli elementi specifici della lista. Ecco un esempio:

```Clojure
(def args (command-line-args))
(println "Il programma è stato eseguito con gli argomenti:" args)
(println "Il primo argomento è:" (nth args 0))
(println "Il secondo argomento è:" (nth args 1))
```
Output:

```
Il programma è stato eseguito con gli argomenti: ["arg1" "arg2"]
Il primo argomento è: arg1
Il secondo argomento è: arg2
```

## Approfondimento

Oltre alla funzione *command-line-args*, possiamo anche utilizzare la libreria *clap*, che ci offre maggiori opzioni per gestire gli argomenti della riga di comando in modo più strutturato. Ad esempio, possiamo definire opzioni con valori obbligatori o facoltativi, impostare delle descrizioni per ogni opzione e gestire gli errori in modo più efficiente. Ecco un esempio di codice utilizzando *clap*:

```Clojure
(require '[clap.core :refer [arg opt parse]])

(def opts (arg &quot;file&quot; :optional true :description &quot;Il file da elaborare&quot;)
          (arg &quot;arg1&quot; :required true :description &quot;Primo argomento&quot;)
          (opt :verbose &quot;-v&quot; :description &quot;Abilita il flag verbose&quot;))

(def arg-map (parse opts))
(println arg-map)
```

Output:

```
{:file "nome_file" :arg1 "primo_argomento" :verbose true}
```

## Vedi anche

- https://clojure.org/reference/miscellaneous_functions#command-line-args
- https://github.com/clojure/clap