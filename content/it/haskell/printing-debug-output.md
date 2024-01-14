---
title:    "Haskell: Stampa di output di debug"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Stampare gli output di debug è una pratica comune tra gli sviluppatori di Haskell. Ciò consente di comprendere meglio il flusso di esecuzione del codice e di individuare eventuali errori.

## Come fare

Per stampare output di debug in Haskell, è possibile utilizzare la funzione `putStrLn` seguita da una stringa di testo all'interno di un blocco `do`. Ad esempio:

```Haskell
main = do
    putStrLn "Inizio programma"
    let x = 5
    putStrLn $ "Valore di x: " ++ show x
    putStrLn "Fine programma"
```

Output:
```
Inizio programma
Valore di x: 5
Fine programma
```

## Approfondimento

La stampa di output di debug può diventare particolarmente utile quando si lavora con funzioni complesse o con dati di tipo composto. In questi casi, è possibile utilizzare la funzione `show` per ottenere una rappresentazione testuale dei valori. Ad esempio:

```Haskell
data Person = Person String Int

instance Show Person where
    show (Person name age) = "Nome: " ++ name ++ ", Età: " ++ show age

main = do
    let p = Person "Mario" 30
    putStrLn $ "Informazioni sulla persona: " ++ show p
```

Output:
```
Informazioni sulla persona: Nome: Mario, Età: 30
```

## Vedi anche

- [Documentazione di Haskell](https://www.haskell.org/documentation/)
- [Funzioni di debugging in Haskell](http://learnyouahaskell.com/input-and-output#hello-world)
- [Approfondimento sulla funzione `show`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:show)