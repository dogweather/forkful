---
title:                "Unione di stringhe"
html_title:           "Haskell: Unione di stringhe"
simple_title:         "Unione di stringhe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Ma perché si dovrebbe mai voler unire due stringhe insieme? La risposta è semplice: per creare una nuova stringa che sia composta da entrambe le stringhe originarie. Questo può essere utile per creare output personalizzati, per manipolare dati o per semplicemente giocare con le stringhe.

## Come fare

Per unire due stringhe, è sufficiente utilizzare l'operatore `++`:

```Haskell
-- Dichiarazione delle stringhe
let stringa1 = "Ciao"
let stringa2 = " mondo!"

-- Unione delle stringhe
let nuovaStringa = stringa1 ++ stringa2

-- Output: "Ciao mondo!"
print nuovaStringa 
```

Notare come l'operatore `++` unisce la prima stringa alla seconda, creando una nuova stringa che contiene entrambe le parole insieme. Questo funziona con qualsiasi tipo di stringa, sia che siano composte da lettere, numeri o caratteri speciali.

## Approfondimento

È importante notare che l'operatore `++` viene utilizzato solo per unire due stringhe insieme. Se si desidera unire più di due stringhe, è necessario utilizzare la funzione `concat`:

```Haskell
-- Dichiarazione delle stringhe
let stringa1 = "Ciao"
let stringa2 = " bello"
let stringa3 = " mondo!"

-- Unione delle stringhe
let nuovaStringa = concat [stringa1, stringa2, stringa3]

-- Output: "Ciao bello mondo!"
print nuovaStringa
```

Inoltre, è possibile utilizzare l'operatore `++` e la funzione `concat` anche per unire diverse tipologie di dati, come ad esempio stringhe e numeri. Tuttavia, è importante assicurarsi che tutti i tipi di dati siano compatibili tra loro per evitare errori in fase di compilazione.

## Vedi anche

Per ulteriori informazioni sulle funzioni di manipolazione delle stringhe in Haskell, puoi consultare i seguenti link:

- [Funzioni di manipolazione delle stringhe in Haskell](https://www.haskell.org/hoogle/?hoogle=string+manipulation)
- [Documentazione ufficiale su concatenamento di stringhe in Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Base.html#v:++)
- [Tutorial di Haskell per principianti](https://www.haskell.org/tutorial/index.html)