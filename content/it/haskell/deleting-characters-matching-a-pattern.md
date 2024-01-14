---
title:                "Haskell: Cancellazione di caratteri corrispondenti a un modello"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

C'è un'operazione comune nella programmazione che spesso ci troviamo ad affrontare: la cancellazione di caratteri che corrispondono a un determinato modello. Ma perché dovremmo farlo? A volte è necessario pulire i dati inseriti dagli utenti, altre volte dobbiamo manipolare stringhe complesse per ottenere i risultati desiderati. In ogni caso, avere gli strumenti per eliminare facilmente i caratteri di cui non abbiamo bisogno può fare la differenza nel nostro lavoro di sviluppo.

## Come fare

Fortunatamente, Haskell offre un modo semplice ed elegante per gestire la cancellazione di caratteri che corrispondono a un modello utilizzando la funzione `filter`. Questa funzione prende come input una funzione di predicato e una lista, e restituisce una nuova lista contenente solo gli elementi che soddisfano il predicato. Per esempio, se volessimo eliminare tutte le vocali da una stringa, potremmo utilizzare il predicato `not . (`elem` "aeiou")`, che restituisce `True` solo quando il carattere non è una vocale, come mostrato di seguito:

```Haskell
filteredString = filter (not . (`elem` "aeiou")) "Ciao, come stai?"
```

L'output di questa esecuzione sarebbe "C, cm st?"

Un altro modo per eliminare i caratteri è utilizzare la funzione `delete` del modulo `Data.List`. Questa funzione prende come input un elemento e una lista e restituisce una nuova lista in cui viene eliminato il primo elemento che corrisponde a quello dato. Utilizzando `delete`, potremmo eliminarlo caratteri `"a"` e `"e"` dalla nostra stringa in questo modo:

```Haskell
deletedString = delete 'a' $ delete 'e' "Ciao, come stai?"
```

E l'output sarebbe lo stesso di quello precedente.

## Approfondimento

Ora che abbiamo visto come implementare rapidamente la cancellazione di caratteri che corrispondono a un determinato modello, è importante comprendere che le funzioni come `filter` e `delete` non modificano direttamente i dati di input, ma creano una nuova lista con i risultati filtrati o eliminati. Pertanto, è necessario assegnare l'output di queste funzioni a una nuova variabile se si vuole accedere ai risultati filtrati o eliminati.

Inoltre, esiste un modo per combinare più funzioni in una sola per ottenere lo stesso risultato. Ad esempio, per eliminare sia le vocali che le virgole dalla stringa, potremmo utilizzare il seguente codice:

```Haskell
eliminatedString = filter (not . (`elem` "aeiou")) . delete ',' "Ciao, come stai?"
```

## Vedi anche

- [Haskell Wiki - Introduzione alle liste](https://wiki.haskell.org/Lists_Introduction)
- [Haskell Hoogle - Riferimento alle funzioni del modulo Data.List](https://hoogle.haskell.org/?hoogle=Data.List)