---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
L'output di debug è un metodo per capire cosa sta facendo un programma, tracciando valori di variabili o flussi di esecuzione. I programmatori lo usano per individuare errori ("bug") nel codice.

## Come fare:
In Haskell, la funzione `print` può essere usata per stampare un output di debug. Ecco un esempio:
```Haskell
main = do
  let x = 7
  print ("Il valore di x è: ", x)
```
Risultato:
```
("Il valore di x è: ", 7)
```
Se volete stampare da una funzione pura, potete utilizzare la funzione `trace` da `Debug.Trace`:
```Haskell
import Debug.Trace

somma :: Int -> Int -> Int
somma x y = trace("somma viene chiamato con " ++ show(x, y)) x + y

main = print(somma 3 4)
```
Risultato:
```
somma viene chiamato con (3,4)
7
```
## Approfondimento 
Storicamente, Haskell era un lisp-like, quindi i `print` e `trace` fanno cose simili alle funzioni di stampa lisp. Ci sono tuttavia alternative per il debug, come il debug interactive con GHCi o l'uso di librerie per aiuto al Debug, come `Debug.SimpleReflect`.

Ricordate, `trace` non è perfetto - esso vìola la trasparenza referenziale (uno dei principi fondanti di Haskell) e non dovrebbe essere usato in codice di produzione. 

##Vedi Anche
- [Haskell Debug Tutorial](https://www.schoolofhaskell.com/school/starting-with-haskell/debugging)
- [Haskell Trace](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)
- [Debugging Haskell Haskell Wiki](https://wiki.haskell.org/Debugging)