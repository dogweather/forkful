---
title:                "Stampa output di debug"
html_title:           "Haskell: Stampa output di debug"
simple_title:         "Stampa output di debug"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler stampare un output di debug. Può essere utile per vedere il valore di una variabile in un determinato punto del tuo codice, per capire come una funzione sta manipolando i dati o semplicemente per verificare se un'azione è stata eseguita correttamente.

## Come farlo
Per stampare un output di debug in Haskell, puoi utilizzare la funzione `print` con il valore che desideri visualizzare all'interno delle parentesi. Ad esempio:

```Haskell
print "Hello world!"

-- Output: "Hello world!"
```

Se vuoi stampare più di un valore, puoi utilizzare la funzione `putStrLn` con una stringa che conterrà tutti i valori separati da spazi. Ad esempio:

```Haskell
putStrLn ("Il mio nome è" ++ nome ++ "e ho" ++ (show età) ++ "anni.")

-- Output: "Il mio nome è Marco e ho 27 anni."
```

Puoi anche combinare stringhe e valori all'interno delle parentesi utilizzando l'operatore `++`, come nel secondo esempio.

## Approfondimento
Se vuoi essere più specifico con l'output di debug, puoi utilizzare la funzione `putStrLn` con la sintassi di formattazione di stringa di Haskell. Ciò ti permetterà di specificare in modo più preciso dove e come vuoi stampare i valori. Ad esempio, se vuoi visualizzare un valore con due cifre decimali, puoi usare la seguente formattazione:

```Haskell
putStrLn ("Il prezzo è: " ++ show prezzo ++ "€")

-- Output: Il prezzo è: 10.50€
```

Puoi anche utilizzare la formattazione di stringa per aggiungere spazi o nuove righe nei tuoi output di debug. Per maggiori informazioni su come utilizzare la formattazione di stringa di Haskell, puoi consultare la documentazione ufficiale.

## Vedi anche
- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Tutorial di Haskell su Codecademy](https://www.codecademy.com/courses/learn-haskell)