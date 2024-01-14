---
title:                "Elm: Concatenare stringhe"
simple_title:         "Concatenare stringhe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione fondamentale nella programmazione, specialmente in linguaggi come Elm. Permette di combinare diverse stringhe per creare un'unica stringa più lunga. Questo è utile per la creazione di output personalizzati o per la manipolazione di dati.

## Come fare

Ecco un semplice esempio di concatenazione di stringhe in Elm:

```Elm
nome = "Marco"
cognome = "Rossi"
saluto = "Ciao, " ++ nome ++ " " ++ cognome ++ "!"
```

L'output di questo codice sarà "Ciao, Marco Rossi!". Come puoi vedere, utilizzando l'operatore "++" è possibile unire più stringhe per creare una nuova stringa. È anche possibile concatenare variabili con stringhe statiche, come nell'esempio sopra.

È importante notare che l'operatore "++" funziona solo con stringhe e non va utilizzato per unire altri tipi di dati. Inoltre, non è possibile concatenare una stringa con un numero, ma è possibile convertire un numero in stringa utilizzando la funzione `toString`.

## Approfondimento

La concatenazione di stringhe è una delle operazioni più frequenti nella programmazione e ci sono alcune considerazioni importanti da tenere a mente. Ad esempio, l'ordine in cui vengono aggiunte le stringhe può influenzare il risultato finale. Se la stringa statica viene aggiunta all'inizio, il codice sarà più efficiente, mentre se viene aggiunta alla fine, potrebbe essere più leggibile.

Inoltre, se si sta lavorando con tante stringhe, potrebbe essere più conveniente utilizzare la funzione `String.concat` che permette di concatenare una lista di stringhe invece di dover utilizzare l'operatore "++" molte volte.

In generale, è importante prestare attenzione alla concatenazione di stringhe poiché può avere un impatto significativo sulle prestazioni del codice.

## Vedi anche

- [Documentazione ufficiale Elm per la concatenazione di stringhe](https://guide.elm-lang.org/strings/concatenation.html)
- [Articolo sull'ottimizzazione della concatenazione di stringhe in Elm](https://medium.com/@dillonkearns/optimize-string-concatenation-in-elm-1a80499f31c6)
- [Esempi pratici di concatenazione di stringhe in Elm](https://www.tutorialspoint.com/elm/elm_string_concat.htm)