---
title:                "Elm: Unione di stringhe"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

In programmazione, a volte è necessario unire più stringhe per creare una nuova stringa che contenga tutte le informazioni necessarie. L'Elm ha una funzione incorporata per gestire questa operazione, chiamata `concat`.

## Come fare

Per concatenare due o più stringhe, possiamo utilizzare la funzione `concat` seguita dalle stringhe da unire, separate da virgole. Ecco un esempio di codice:

```Elm
concat "Ciao" "a" "tutti" --> "Ciao a tutti"
```

Se vogliamo aggiungere una stringa vuota come separatore, possiamo utilizzare la funzione `concatWith`.

```Elm
concatWith " " "Ciao" "a" "tutti" --> "Ciao a tutti"
```

## Approfondimento

La funzione `concat` in realtà accetta una lista di stringhe invece di una serie di argomenti separati. Questo significa che possiamo anche utilizzare la funzione `List.concat` per ottenere lo stesso risultato:

```Elm
List.concat ["Ciao", "a", "tutti"] --> "Ciao a tutti"
```

Inoltre, l'Elm ha anche una funzione `String.join` che ci consente di unire una lista di stringhe utilizzando un separatore specifico. Ecco un esempio:

```Elm
String.join " " ["Ciao", "a", "tutti"] --> "Ciao a tutti"
```

## Vedi anche

- Documentazione su `concat`: https://package.elm-lang.org/packages/elm/core/latest/String#concat
- Documentazione su `List.concat`: https://package.elm-lang.org/packages/elm/core/latest/List#concat
- Documentazione su `String.join`: https://package.elm-lang.org/packages/elm/core/latest/String#join