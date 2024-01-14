---
title:    "Clojure: Estrazione di sottostringhe"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe è un'operazione fondamentale quando si lavora con stringhe in Clojure. Ciò consente di ottenere parti specifiche di una stringa e utilizzarle per scopi specifici, come la manipolazione o la ricerca di informazioni. In questo articolo, esploreremo come estrarre sottostringhe in modo efficiente utilizzando il linguaggio Clojure.

## Come Fare

Per estrarre una sottostringa in Clojure, è possibile utilizzare la funzione `substring` che prende in input la stringa di origine, la posizione di inizio e la posizione di fine della sottostringa desiderata. Ad esempio, se vogliamo estrarre la prima metà di una stringa, possiamo utilizzare il seguente codice:

```Clojure
(def parola "ciao")
(substring parola 0 2)
```

Questo ci restituirà la sottostringa "ci". Possiamo anche utilizzare numeri negativi per indicare le posizioni di fine count back dalla fine della stringa, come mostrato di seguito:

```Clojure
(def parola "mondo")
(substring parola 0 -1)
```

Questo ci restituirà la sottostringa "mond".

## Deep Dive

La funzione `substring` in Clojure restituisce una stringa nuova anziché modificare la stringa di origine, poiché le stringhe sono immutabili nel linguaggio. Inoltre, è possibile utilizzare la funzione `subs` per estrarre una sottostringa utilizzando un indice di inizio e una lunghezza anziché una posizione di fine. Ad esempio, se vogliamo estrarre gli ultimi due caratteri di una stringa, possiamo utilizzare il seguente codice:

```Clojure
(def parola "casa")
(subs parola (- (count parola) 2) (count parola))
```

Questo ci restituirà la sottostringa "sa". È importante notare che, mentre `substring` utilizza indici inclusivi, `subs` utilizza indici esclusivi.

## Vedi Anche
- [Documentazione sulle stringhe Clojure](https://clojure.org/reference/strings)
- [Estrarre sottostringhe in altri linguaggi di programmazione](https://www.tutorialspoint.com/finds-sub-strings-in-different-languages)
- [Video tutorial su come estrarre sottostringhe in Clojure](https://www.youtube.com/watch?v=Lqr9GYfIhTQ)