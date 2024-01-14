---
title:                "Clojure: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe è un'operazione comune nella programmazione, soprattutto quando si lavora con stringhe di testo più lunghe. Ciò consente di ottenere parti specifiche di una stringa e manipolarle in modi diversi. In Clojure, ci sono diversi modi per estrarre sottostringhe, ognuno con i suoi vantaggi e casi d'uso. Continua a leggere per scoprire come farlo!

## Come Fare

Ci sono due funzioni principali per estrarre sottostringhe in Clojure: `subs` e `substring`. Entrambe richiedono la stringa di origine come primo argomento, seguita dalla posizione di inizio e di fine della sottostringa desiderata. Tuttavia, ci sono alcune differenze chiave tra le due.

Considera il seguente esempio:

```Clojure
(def test-string "Ciao, sono un programmatore!")
```

### Utilizzando `subs`

Con la funzione `subs`, è possibile specificare l'indice di inizio e di fine come secondo e terzo argomento rispettivamente. Tieni presente che l'indice di fine è esclusivo, il che significa che il carattere corrispondente all'indice non viene incluso nella sottostringa risultante.

```Clojure
(subs test-string 6 8) ; restituisce "so"
```

Inoltre, `subs` ha la capacità di accettare indici negativi, che rappresentano il conteggio all'indietro dalla fine della stringa.

```Clojure
(subs test-string -6 -2) ; restituisce "mato"
```

### Utilizzando `substring`

D'altra parte, `substring` richiede l'indice di inizio come secondo argomento, ma invece dell'indice di fine, richiede un terzo argomento che rappresenta la lunghezza della sottostringa. In questo caso, il carattere corrispondente all'indice di fine è incluso nella sottostringa risultante.

```Clojure
(substring test-string 0 4) ; restituisce "Ciao"
```

Anche in questo caso, `substring` può accettare indici negativi per entrambi gli argomenti.

```Clojure
(substring test-string -11 -1) ; restituisce "programmatore"
```

## Approfondimento

Oltre ai metodi descritti sopra, Clojure ha a disposizione altre funzioni per estrarre sottostringhe. Ad esempio, `subseq` accetta gli stessi parametri di `subs`, ma restituisce una sequenza di caratteri anziché una stringa, permettendo così di utilizzare metodi di manipolazione di sequenze.

```Clojure
(subseq test-string 5 10) ; restituisce (\, \s \o \n \o)
```

Inoltre, Clojure ha anche il concetto di "pattern di ricerca" da utilizzare con la funzione `re-find` per trovare il primo match di una sottostringa all'interno della stringa di origine.

```Clojure
(re-find #"programmatore" test-string) ; restituisce "programmatore"
```

## Vedi Anche

- [Documentazione ufficiale di Clojure su `subs`](https://clojuredocs.org/clojure.core/subs)
- [Documentazione ufficiale di Clojure su `substring`](https://clojuredocs.org/clojure.core/substring)
- [Documentazione ufficiale di Clojure su `subseq`](https://clojuredocs.org/clojure.core/subseq)
- [Documentazione ufficiale di Clojure su `re-find`](https://clojuredocs.org/clojure.core/re-find)