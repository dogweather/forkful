---
title:    "Elm: Capitalizzare una stringa"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è un'operazione molto comune nella programmazione, specialmente quando si lavora con testi. Potrebbe essere necessario farlo per formattare correttamente un titolo o per uniformare l'aspetto del testo in un'utilità di formattazione. In Elm, esiste un modo semplice e diretto per capitalizzare una stringa, che verrà mostrato nella sezione successiva.

## Come fare

Per capitalizzare una stringa in Elm, possiamo utilizzare una funzione integrata chiamata `String.toUpper`. Questa funzione accetta una stringa come argomento e restituisce la stessa stringa con tutte le lettere maiuscole. Di seguito è riportato un esempio di codice con un input e un output di esempio:

```Elm
String.toUpper "ciao" --> "CIAO"
```

Se desideriamo capitalizzare solo la prima lettera di una stringa, possiamo usare la funzione `String.capitalize`, come mostrato qui sotto:

```Elm
String.capitalize "elm" --> "Elm"
```

## Approfondimento

Per comprendere meglio come funzionano queste funzioni, è utile capire che in Elm le stringhe sono immutabili, il che significa che una volta create non possono essere modificate. Ciò significa che le funzioni come `String.toUpper` e `String.capitalize` non modificheranno direttamente la stringa originale, ma restituiranno una nuova stringa con le modifiche applicate. Questo è un concetto importante da tenere a mente quando si lavora con le stringhe in Elm.

Inoltre, queste funzioni possono essere utilizzate anche su stringhe vuote o contenenti solo numeri. In questi casi, verrà restituito semplicemente la stessa stringa senza alcuna modifica.

## Vedi anche

- Documentazione ufficiale di Elm su `String`
https://package.elm-lang.org/packages/elm/core/latest/String
- Guida alla programmazione funzionale in Elm
https://guide.elm-lang.org/functional_programming/
- Esempi di codice con Elm
https://github.com/elm/projects

Grazie per aver letto questo articolo! Spero che ti sia stato utile per imparare a capitalizzare le stringhe in Elm. Continua a esplorare questa potente lingua di programmazione funzionale e buona scrittura di codice!