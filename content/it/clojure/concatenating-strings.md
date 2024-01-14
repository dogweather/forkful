---
title:    "Clojure: Concatenazione di stringhe"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché 
In questo articolo parleremo di una funzionalità fondamentale della programmazione Clojure, ovvero la concatenazione di stringhe. Con questa operazione, è possibile unire una o più stringhe per creare un'unica stringa, utile in molti contesti della programmazione. Scopriremo insieme come utilizzare questa funzionalità e approfondiremo alcuni aspetti tecnici.

## Come fare 
Per concatenare stringhe in Clojure, possiamo utilizzare la funzione `str`. Questa funzione, dato un numero variabile di input, unisce tutte le stringhe in un'unica stringa. Vediamo un esempio di utilizzo:

```Clojure
(str "Ciao" "a" "tutti") ;output: "Ciaoa tutti"
(str "Il mio numero preferito è" 7) ;output: "Il mio numero preferito è 7"
```

Come possiamo notare, la funzione `str` consente di unire diverse tipologie di dati, non solo stringhe. Inoltre, se uno dei dati di input è vuoto o una lista vuota, questo verrà ignorato senza causare errori.

## Approfondimenti 
Se vogliamo concatenare una grande quantità di stringhe, potremmo pensare di utilizzare il classico operatore `+`. Tuttavia, in Clojure è sconsigliato utilizzare questo operatore per motivi di efficienza. Infatti, `+` è pensato principalmente per l'addizione di numeri, mentre `str` è ottimizzato per la concatenazione di stringhe.

Un'altra funzione interessante per la concatenazione di stringhe è `join`. Questa funzione consente di unire una lista di stringhe utilizzando un separatore specificato. Vediamo un esempio:

```Clojure
(join ", " ["Pizza" "Pasta" "Parmigiana"]) ;output: "Pizza, Pasta, Parmigiana"
(join " - " ["Mozzarella" "Pomodoro" "Basilico"]) ;output: "Mozzarella - Pomodoro - Basilico"
```

Infine, è importante ricordare che Clojure utilizza delle sequenze immutabili, quindi quando concateniamo stringhe, in realtà stiamo creando nuove stringhe senza modificare quelle esistenti. Questo aspetto è importante da considerare quando lavoriamo con grandi quantità di dati per evitare problemi di memoria.

## Vedi anche 
- [Documentazione ufficiale di Clojure sulla funzione str](https://clojure.org/reference/data_structures#_clojure_string_conversion_functions)
- [Guida alla concatenazione di stringhe in Clojure](https://www.baeldung.com/clojure/string-concatenation-join)
- [Ulteriori nozioni sulla gestione delle sequenze in Clojure](https://en.wikibooks.org/wiki/Clojure_Programming/Basic_Scala_Datatype_Refresher#Sequences)