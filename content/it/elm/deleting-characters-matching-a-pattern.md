---
title:                "Elm: Cancellazione dei caratteri corrispondenti a un modello"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, durante lo sviluppo di un'applicazione, ci troviamo di fronte alla necessità di eliminare determinati caratteri che corrispondono ad uno specifico pattern. Questo potrebbe essere necessario per ripulire i dati in ingresso o per soddisfare alcuni requisiti di formattazione. Nel linguaggio di programmazione Elm, esiste una semplice e efficiente funzione che ci permette di eliminare questi caratteri.

## Come fare

Per eliminare caratteri che corrispondono ad un determinato pattern, possiamo utilizzare la funzione `String.filter` in Elm. Questa funzione accetta come argomenti una funzione di predizione e una stringa di input e restituisce una nuova stringa contenente solo i caratteri che soddisfano il pattern specificato.

Ecco un esempio di come utilizzare la funzione `String.filter` per eliminare le vocali da una stringa:

```Elm
String.filter (\char -> not (List.member char ['a', 'e', 'i', 'o', 'u'])) "Ciao amici"
```

L'output di questo codice sarà `"C c"`, poiché sono state rimosse tutte le vocali dalla stringa originale. Possiamo anche utilizzare questa funzione per rimuovere tutti i numeri da una stringa, semplicemente passando una funzione di predizione diversa.

```Elm
String.filter (\char -> not (Char.isDigit char)) "abc123def"
```

Anche in questo caso, l'output sarà `"abcdef"`, poiché tutti i numeri sono stati eliminati dalla stringa originale.

## Approfondimento

Oltre alla funzione `String.filter`, Elm offre altre funzioni utili per manipolare le stringhe. Ad esempio, la funzione `String.replace` ci permette di sostituire una stringa con un'altra all'interno di un'altra stringa.

Ecco un esempio di come utilizzare `String.replace` per sostituire la stringa "ciao" con "hello" in una stringa di input:

```Elm
String.replace "ciao" "hello" "Ciao amici"
```

L'output di questo codice sarà `"Hello amici"`. Inoltre, Elm offre anche una serie di funzioni per lavorare con i caratteri, come `Char.isLetter` per verificare se un carattere è una lettera e `Char.toUpper` per convertire un carattere in maiuscolo.

## Vedi anche

- Documentazione ufficiale di Elm sulle stringhe: https://package.elm-lang.org/packages/elm/core/latest/String
- Tutorial su come utilizzare le funzioni `String.filter` e `String.replace`: https://guide.elm-lang.org/strings/
- Esempi di codice per manipolare le stringhe in Elm: https://github.com/elm/projects/blob/master/beginner-projects/strings.md