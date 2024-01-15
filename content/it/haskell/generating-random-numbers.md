---
title:                "Generazione di numeri casuali"
html_title:           "Haskell: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'operazione comune nei linguaggi di programmazione, inclusa la versione attuale di Haskell. Ciò può essere utile in diverse situazioni come simulazioni, giochi o generazione di dati casuali per testare il codice.

## Come fare

Per generare numeri casuali in Haskell, è necessario importare il modulo `System.Random` utilizzando il comando `import System.Random`. Quindi, utilizzando la funzione `randomRIO`, è possibile generare un numero casuale all'interno di un intervallo specificato. Ad esempio, se si vuole generare un numero casuale compreso tra 1 e 10, è possibile utilizzare il seguente codice:

```Haskell
import System.Random

main = do
  numeroCasuale <- randomRIO (1, 10)
  print numeroCasuale
```

L'esempio di output potrebbe essere:

```Haskell
5
```

## Approfondimento

Haskell utilizza il concetto di "generatore di numeri casuali" per generare numeri casuali. Questo generatore viene creato utilizzando la funzione `mkStdGen` che prende come input un numero intero. Inoltre, è possibile ottenere la stessa sequenza di numeri casuali utilizzando lo stesso generatore. Ad esempio, il codice seguente genera la stessa sequenza di numeri casuali ogni volta che viene eseguito:

```Haskell
import System.Random

main = do
  let gen = mkStdGen 42 
  let (numero1, _) = random gen :: (Int, StdGen)
  let (numero2, _) = random gen :: (Int, StdGen)
  print numero1
  print numero2
```

L'output sarà:

```Haskell
-2
-2
```

Inoltre, è possibile generare numeri casuali di altri tipi di dati, come `Double` o `Bool`. Per fare ciò, è necessario specificare il tipo di dati nella chiamata della funzione `random`. Ad esempio:

```Haskell
import System.Random

main = do
  numeroCasuale <- randomRIO (1.0, 10.0) :: IO Double
  print numeroCasuale
```

Questo esempio genererà un numero casuale di tipo `Double` compreso tra 1.0 e 10.0.

## Vedi anche

- Documentazione ufficiale di Haskell sul modulo `System.Random`: https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Random.html
- Tutorial su come generare numeri casuali in Haskell: https://wiki.haskell.org/Random_numbers