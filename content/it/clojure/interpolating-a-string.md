---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolazione di stringhe in Clojure

## Cosa & Perché?

L'interpolazione delle stringhe permette di inserire valori di variabili all'interno di una stringa. Lo facciamo per creare stringhe dinamiche senza dover concatenare costantemente stringhe e variabili.

## Come fare:

In Clojure, possiamo interpolare stringhe utilizzando la funzione `format`. Ecco un esempio:

```Clojure
(let [nome "Mario"]
  (format "Ciao, %s!" nome))
```

Output:

```Clojure
"Ciao, Mario!"
```

## Approfondimento

Historicamente, l'interpolazione delle stringhe non era di default in Clojure, richiedendo invece l'utilizzo del metodo `format`. Tuttavia, con la crescente popolarità di Clojure, diverse librerie di terze parti hanno cominciato a offrire funzioni di interpolazione delle stringhe.

Un'alternativa all'uso del metodo `format` è la libreria `clojure.string/replace`. Questa libreria consente di sostituire una parte di una stringa con una funzione o una mappa. Questo può essere utile in scenari complessi dove `format` potrebbe non essere sufficiente.

Clojure implementa l'interpolazione di stringhe utilizzando le stringhe di formato Java. A seconda del tipo di dato da interpolare, differenti specificatori di formato vengono utilizzati.

## Da Vedere Anche 

Per ulteriori informazioni su come utilizzare l'interpolazione di stringhe in Clojure, consulta questi utili link:

- Stack Overflow: [How to do string interpolation in Clojure?](https://stackoverflow.com/questions/6414120/how-to-do-string-interpolation-in-clojure)
- Clojure Docs: [clojure.core/format](https://clojuredocs.org/clojure.core/format)
- Clojure Docs: [clojure.string/replace](https://clojuredocs.org/clojure.string/replace)