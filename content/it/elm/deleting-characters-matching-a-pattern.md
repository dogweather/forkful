---
title:                "Eliminazione di caratteri corrispondenti a un modello"
html_title:           "Elm: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Il motivo principale per cui si potrebbe voler eliminare i caratteri corrispondenti a un certo modello è per semplificare il proprio codice e renderlo più leggibile. Inoltre, questo può aiutare ad evitare errori di digitazione e facilitare la manutenzione del codice.

## Come Fare

Per eliminare i caratteri che corrispondono a un modello specifico in Elm, è possibile utilizzare la funzione `String.filter`. Questa funzione prende come argomento un predicato, che è una funzione che restituisce un valore booleano. Il predicato viene applicato ad ogni carattere della stringa e se restituisce `True`, il carattere viene mantenuto, altrimenti viene eliminato.

Un esempio di come utilizzare `String.filter` per eliminare tutte le vocali da una stringa può essere il seguente:

```Elm
result : String
result =
  "Ciao a tutti"
  |> String.filter (\c -> not (String.member c "aeiou"))
```

In questo codice, la funzione `String.member` viene utilizzata per verificare se un carattere è contenuto nella stringa "aeiou". Utilizzando `not` prima di questa espressione, otteniamo il contrario e quindi tutti i caratteri diversi dalle vocali verranno mantenuti.

L'output di `result` sarà `"C ttt".

## Approfondimento

Un aspetto interessante da notare è che la funzione `String.filter` restituisce una nuova stringa, invece di modificare quella originale. Questo è importante perché in Elm le stringhe sono immutabili, quindi ogni operazione su di esse crea sempre una nuova istanza.

Inoltre, è possibile utilizzare `String.filter` insieme ad altre funzioni come `String.map` per ottenere risultati più complessi. Ad esempio, si potrebbe voler mantenere solo le consonanti, ma convertire le vocali in maiuscolo. In questo caso, il codice potrebbe essere il seguente:

```Elm
result : String
result =
  "Ciao a tutti"
  |> String.filter (\c -> not (String.member c "aeiou"))
  |> String.map (\c -> if c == Char.toLower c then Char.toUpper c else c)
```

L'output di `result` sarà quindi `"C TTT".

## Vedi Anche

- [Documentazione ufficiale di Elm sulle stringhe](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Altro esempio di utilizzo di `String.filter` in Elm](https://elmprogramming.com/deleting-characters-from-string-in-elm.html)