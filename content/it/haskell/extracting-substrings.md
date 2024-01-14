---
title:                "Haskell: Estrazione di sottostringhe"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

La manipolazione delle stringhe è una parte essenziale della programmazione estrarre delle sottostringhe può essere utile in molti casi, ad esempio per ottenere informazioni specifiche da un input o per formattare il testo in modo più leggibile.

## Come fare

Per estrarre sottostringhe in Haskell, abbiamo a disposizione la funzione `take` che restituisce i primi n caratteri di una stringa e la funzione `drop` che invece elimina i primi n caratteri. Ad esempio:

```Haskell
-- Esempio di utilizzo di take
take 3 "Ciao a tutti" 
-- Output: "Cia"

-- Esempio di utilizzo di drop
drop 7 "Ciao a tutti" 
-- Output: "tutti"
```

Possiamo anche utilizzare la funzione `takeWhile` per estrarre i caratteri fino a quando una certa condizione non viene più verificata, come ad esempio:

```Haskell
-- Esempio di utilizzo di takeWhile
takeWhile (/=' ') "Ciao a tutti"
-- Output: "Ciao"
```

Inoltre, possiamo combinare queste funzioni per ottenere sottostringhe di diversa lunghezza e con condizioni specifiche.

## Approfondimento

In Haskell, le stringhe sono trattate come liste di caratteri, quindi possiamo utilizzare le stesse funzioni per manipolarle. Inoltre, esistono anche funzioni specifiche come `substring` che ci permettono di estrarre una sottostringa partendo da una determinata posizione e di una determinata lunghezza.

Inoltre, è importante prestare attenzione alla gestione degli errori quando si estraggono substranghe, poiché se la sottostringa richiesta è più lunga della stringa originale, si potrebbe verificare un errore di indice fuori dai limiti.

## Vedi anche

- [Haskell String Functions](https://www.tutorialspoint.com/haskell/haskell_string_functions.htm)
- [Haskell Strings](https://wiki.haskell.org/Strings)