---
title:                "Clojure: Concatenazione di stringhe"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

Perché: Perché dovresti utilizzare la concatenazione di stringhe in Clojure?

La concatenazione di stringhe è un'operazione comune nella programmazione e può essere utile per combinare testo o variabili in un'unica stringa. In Clojure, questo è fatto con la funzione `str`, che prende una serie di argomenti e li concatena in una stringa.

## Come Fare

Per utilizzare la concatenazione di stringhe in Clojure, possiamo usare la funzione `str` seguita dagli argomenti che vogliamo concatenare. Ad esempio, se vogliamo creare una stringa che contenga il nostro nome, possiamo farlo in questo modo:

```Clojure
(str "Il mio nome è " "Maria")
```

Questo ci darà una stringa di output che dice "Il mio nome è Maria". Possiamo anche usare la concatenazione di più parametri, come numeri o variabili, come segue:

```Clojure
(def nome "Marco")
(def eta 30)
(str "Il mio nome è " nome " e ho " eta " anni.")
```

Questo ci darà una stringa che dice "Il mio nome è Marco e ho 30 anni." Come puoi vedere, `str` sarà in grado di convertire automaticamente tipi diversi in stringhe.

## Approfondimento

In Clojure, la funzione `str` è in realtà un'interfaccia per `StringBuilder`, che è uno strumento più efficiente di concatenazione di stringhe. Invece di creare una nuova stringa ogni volta che viene chiamata la funzione `str`, `StringBuilder` aggiunge semplicemente il nuovo contenuto alla fine di una stringa esistente, riducendo così il numero di allocazioni di memoria.

Inoltre, Clojure offre anche la macro `clojure.core/format`, che è utile per formattare le stringhe in modo più complesso e personalizzato.

## Vedi Anche

Per ulteriori informazioni sulla concatenazione di stringhe in Clojure, puoi consultare questi link utili:

- [Documentazione ufficiale su `str`](https://clojuredocs.org/clojure.core/str)
- [Tutorial su come concatenare stringhe in Clojure](https://www.braveclojure.com/core-functions-in-depth/#str)
- [Ulteriori informazioni su `StringBuilder`](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Documentazione su `format`](https://clojuredocs.org/clojure.core/format)