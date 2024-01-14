---
title:    "Gleam: Capitalizzazione di una stringa"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Perché

Sembra un compito semplice, giusto? Capitalizzare una stringa significa semplicemente rendere maiuscola la prima lettera. Ma perché dovresti farlo? Beh, ci sono diverse ragioni.

Innanzitutto, capitalizzare una stringa può migliorare l'estetica del tuo codice. Immagina di avere una variabile che contiene il nome di una persona, se il nome è scritto in maiuscolo sarà più facile da leggere e distinguere all'interno del codice.

Inoltre, capitalizzare una stringa può essere utile quando si lavora con dati di input. In alcuni casi, è necessario assicurarsi che la prima lettera di ogni parola sia maiuscola per una corretta formattazione dei dati.

## Come Fare

Per capitalizzare una stringa in Gleam, possiamo utilizzare il modulo `String` e la funzione `capitalize`:

```Gleam
import String

let nome = "mario"
let nome_cap = String.capitalize(nome)
```

Il risultato di questo codice sarà una nuova variabile `nome_cap` con il valore "Mario".

Ma cosa succede se la nostra stringa ha più di una parola? In questo caso, possiamo utilizzare la funzione `titlecase` per capitalizzare ogni parola nella stringa:

```Gleam
import String

let stringa = "ciao come stai"
let stringa_cap = String.titlecase(stringa)
```

Il risultato di questo codice sarà una nuova variabile `stringa_cap` con il valore "Ciao Come Stai".

È importante notare che queste funzioni non alterano la stringa originale, ma restituiscono una nuova stringa capitalizzata.

## Deep Dive

Se vuoi approfondire il concetto di capitalizzazione delle stringhe, ci sono alcune cose che dovresti tenere a mente.

Innanzitutto, la funzione `capitalize` non solo rende maiuscola la prima lettera, ma trasforma anche tutte le altre lettere in minuscolo. Se vuoi mantenere le lettere originali nella posizione in cui si trovano, puoi utilizzare la funzione`upper_case_first`:

```Gleam
import String

let stringa = "pROGRAMMAZIONE"
let stringa_cap = String.upper_case_first(stringa)
```

Il risultato di questo codice sarà una nuova variabile `stringa_cap` con il valore "PROGRAMMAZIONE".

Inoltre, è importante notare che queste funzioni utilizzano le regole di capitalizzazione della lingua inglese. Se devi lavorare con stringhe in altre lingue, potresti dover eseguire alcune operazioni aggiuntive per gestire le lettere accentate o le regole di capitalizzazione specifiche della lingua.

## Vedi Anche

- Documentazione del modulo `String` in Gleam: https://gleam.run/modules/String.html
- Approfondimenti sulle funzioni di manipolazione delle stringhe: https://gleam.run/cookbook/strings.html
- Impara Gleam: https://gleam.run/learn/