---
title:    "Gleam: Cercando e sostituendo il testo"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Scegliere il linguaggio di programmazione Gleam non solo offre una sintassi pulita e intuitiva, ma anche una vasta gamma di funzionalità pratiche. Tra queste, una delle più utili è la capacità di cercare e sostituire testo all'interno dei nostri script. Vediamo insieme come farlo!

## Come fare

È molto semplice utilizzare la funzione di ricerca e sostituzione di Gleam. Possiamo farlo utilizzando il metodo `String.replace()` e passando come argomenti il testo da cercare e il testo da sostituire. Ad esempio, se volessimo sostituire la parola "cane" con "gatto" in una stringa, il codice sarebbe il seguente:

```Gleam
let stringa = "Il mio cane è un cucciolo di razza."
let nuova_stringa = String.replace(stringa, "cane", "gatto")
```

Il risultato sarebbe la stringa "Il mio gatto è un cucciolo di razza.", che ci conferma che la funzione ha fatto il suo lavoro.

Possiamo anche utilizzare le espressioni regolari per raffinare la nostra ricerca e sostituzione. Ad esempio, se volessimo sostituire tutte le vocali con un asterisco, il codice sarebbe il seguente:

```Gleam
let stringa = "Questa è una frase."
let nuova_stringa = String.replace(stringa, `[aeiou]`, "*")
```

Il risultato sarebbe la stringa "Q*st* * *n* fr*s*.", dove le vocali sono state sostituite con l'asterisco.

## Approfondimento

La funzione `String.replace()` può essere utilizzata anche per raccogliere informazioni sulla sostituzione effettuata. Possiamo farlo passando come terzo argomento una funzione di callback che riceve in input il testo sostituito e restituisce il nuovo testo. Ad esempio, se volessimo contare quante volte viene effettuata una sostituzione, il codice sarebbe il seguente:

```Gleam
let stringa = "Oggi è una bella giornata."
let conteggio_sostituzioni = ref(0)

let nuova_stringa = String.replace(stringa, "bella", fn (_, _) => {
    conteggio_sostituzioni := conteggio_sostituzioni + 1
    "splendida"
})
```

In questo caso, il codice controlla quante volte la parola "bella" viene sostituita con "splendida" e lo memorizza nella variabile `conteggio_sostituzioni`.

## Vedi anche

- Documentazione ufficiale di Gleam sull'utilizzo di `String.replace()`: https://gleam.run/documentation/math/replace
- Esempi avanzati di utilizzo della funzione di ricerca e sostituzione in Gleam: https://gist.github.com/alturman/5abb6d02c116deae6fc690ab06030bd1
- Un tutorial completo sulla gestione di stringhe in Gleam: https://gleam.run/documentation/core-modules/string-functions