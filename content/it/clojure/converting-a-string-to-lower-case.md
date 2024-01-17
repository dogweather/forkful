---
title:                "Convertire una stringa in minuscolo"
html_title:           "Clojure: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

CIAO AMICI!

## Cosa & Perché?

Convertire una stringa in minuscolo è il processo di trasformare tutte le lettere di una stringa in caratteri minuscoli. I programmatori spesso lo fanno per uniformare il formato delle stringhe e rendere più facile la ricerca e il confronto tra di loro.

## Come:

La maggior parte dei programmatori utilizza la funzione "lower-case" nella libreria clojure.string per convertire una stringa in minuscolo. Ecco un esempio di come farlo:

```Clojure
(require '[clojure.string :as str])

(def stringa "CIAO AMICI!")
(str/lower-case stringa)
```

Questo produrrà l'output "ciao amici!". 

Puoi anche utilizzare la funzione "to-lower-case" nella libreria clojure.data.string per ottenere lo stesso risultato. Ecco un esempio:

```Clojure
(require '[clojure.data.string :as str])

(def stringa "CIAO AMICI!")
(str/to-lower-case stringa)
```

Il risultato sarà sempre "ciao amici!".

## Approfondimento:

La conversione delle stringhe in minuscolo ha una lunga storia nella programmazione. Originariamente, era utilizzata principalmente per rendere più facile la ricerca e il confronto tra stringhe, ma oggi è diventata una pratica comune per uniformare il formato delle stringhe e renderle più leggibili.

Un'alternativa alla funzione "lower-case" è l'utilizzo di espressioni regolari per sostituire le lettere maiuscole con quelle minuscole. Tuttavia, questa soluzione richiede più codice e spesso viene preferita la semplicità e l'efficienza della funzione "lower-case".

Per quanto riguarda l'implementazione della conversione delle stringhe in minuscolo, la funzione "lower-case" utilizza il metodo "toLowerCase()" della classe java.lang.String per eseguire la conversione. Nel caso della funzione "to-lower-case", viene utilizzato il metodo "toLowerCase()" della classe java.lang.Character.

## Vedi anche:

- Documentazione della funzione "lower-case": https://clojuredocs.org/clojure.string/lower-case
- Documentazione della funzione "to-lower-case": https://clojuredocs.org/clojure.data.string/to-lower-case