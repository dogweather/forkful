---
title:    "Clojure: Eliminazione di caratteri corrispondenti a un modello"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler eliminare dei caratteri che corrispondono ad un certo pattern all'interno del tuo codice Clojure. Potresti aver inserito dei caratteri indesiderati per sbaglio, o forse vuoi pulire il tuo codice e renderlo più leggibile. In ogni caso, imparare come eliminare questi caratteri è un'abilità utile per qualsiasi programmatore.

## Come Fare
	
Per eliminare i caratteri che corrispondono ad un certo pattern in Clojure, puoi utilizzare la funzione `clojure.string/replace`. Questa funzione accetta tre argomenti: la stringa di input, il pattern da cercare e la stringa con cui sostituire il pattern. Ad esempio, se vogliamo eliminare tutte le vocali dalla parola "programmare", il codice sarebbe il seguente:

```Clojure
(clojure.string/replace "programmare" #"[aeiou]" "")
```

Questo ci darà come risultato la stringa "prgrmmr". Puoi anche utilizzare espressioni regolari più complesse per eliminare specifici pattern di caratteri.

## Approfondimento

Quando si tratta di eliminare caratteri in Clojure, è importante comprendere bene le espressioni regolari. Queste espressioni sono sequenze di caratteri che descrivono un pattern di testo, e possono essere utilizzate con funzioni come `clojure.string/replace` per effettuare sostituzioni in una stringa.

Una delle espressioni regolari più comuni è il wildcard, rappresentato dal punto interrogativo (?), che corrisponde a qualsiasi singolo carattere. Ad esempio, se vogliamo eliminare tutte le consonanti dalla parola "programmare", possiamo utilizzare il seguente pattern: #"?".

Ecco un esempio completo:

```Clojure
(clojure.string/replace "programmare" #"?" "")
```

Questo ci darà come risultato la stringa "aieae".

## Vedi Anche

- [Documentazione ufficiale di Clojure sulle espressioni regolari](https://clojure.org/reference/regular_expressions)
- [Esempi di espressioni regolari per eliminare caratteri in Clojure](https://dev.to/gskayhan/regexp-in-clojure-language-2lnc)
- [Guida rapida alle espressioni regolari in Clojure](https://www.ibm.com/support/knowledgecenter/SSWU4L/RegularExpressions.html)