---
title:    "Haskell: Concatenazione di stringhe"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una pratica comune nella programmazione, soprattutto quando si lavora con testi e dati di carattere testuale. In Haskell, è possibile utilizzare diversi operatori e funzioni per concatenare stringhe e creare output di testo più complessi.

## Come fare

Per concatenare le stringhe in Haskell, è possibile utilizzare l'operatore `++` o la funzione `concat`. Ad esempio, possiamo unire due stringhe in una sola utilizzando l'operatore `++` come segue:

```Haskell
"Benvenuto" ++ " nel mondo della programmazione"  
```
Questo produrrà l'output: "Benvenuto nel mondo della programmazione".

Inoltre, possiamo concatenare più stringhe utilizzando la funzione `concat` come mostrato nell'esempio seguente:

```Haskell
concat ["Ciao", " ", "Mondo", "!"]  
```
Questo produrrà l'output: "Ciao Mondo!".

## Approfondimento

La concatenazione di stringhe in Haskell può essere fatta anche utilizzando il metodo `show` per convertire tipi di dati diversi in stringhe e unirli con l'operatore `++`.

Ad esempio, possiamo unire una stringa con il valore di un intero utilizzando il seguente schema:

```Haskell
"Oggi è il " ++ show 14 ++ " di agosto."  
```
Questo produrrà l'output: "Oggi è il 14 di agosto."

Inoltre, è importante notare che la concatenazione di stringhe può essere eseguita anche con valori di tipo `Char`. In questo caso, è sufficiente utilizzare l'operatore `:` per aggiungere un carattere alla fine della stringa, come mostrato di seguito:

```Haskell
"Buon" ++ 'a' ++ "giorno"  
```
Questo produrrà l'output: "Buongiorno".

## Vedi anche
- [Haskell String Concatenation](https://wiki.haskell.org/Concatenation)
- [Learn You a Haskell for Great Good! - String Introduction](http://learnyouahaskell.com/starting-out#strings-introduction)