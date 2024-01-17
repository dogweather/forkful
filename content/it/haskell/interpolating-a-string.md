---
title:                "Interpolare una stringa"
html_title:           "Haskell: Interpolare una stringa"
simple_title:         "Interpolare una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

Cosa e Perché?
L'interpolazione di una stringa è un modo per sostituire delle parti di una stringa con variabili o valori durante l'esecuzione del programma. I programmatori lo fanno per rendere più dinamiche le stringhe e per evitare di dover creare molteplici versioni di una stessa stringa.

Come fare:
```Haskell
-- Esempio di interpolazione di una stringa utilizzando le variabili
nome = "Mario"
messaggio = "Ciao, sono " ++ nome ++ "!"
-- Output: "Ciao, sono Mario!" 
```

```Haskell
-- Esempio di interpolazione di una stringa utilizzando i valori
eta = 25
messaggio = "Ho " ++ show eta ++ " anni."
-- Output: "Ho 25 anni."
```

```Haskell
-- Esempio di interpolazione di una stringa con più valori
valore1 = 10
valore2 = 5
messaggio = "La somma di " ++ show valore1 ++ " e " ++ show valore2 ++ " è " ++ show (valore1 + valore2) ++ "."
-- Output: "La somma di 10 e 5 è 15."
```

Deep Dive:
L'interpolazione di una stringa è una tecnica comune nei linguaggi di programmazione moderni, ma è stato introdotto per la prima volta in linguaggi come AWK e Perl negli anni '80. Alcune alternative a questa tecnica sono l'utilizzo di formattazione di stringhe o di funzioni di sostituzione. Nell'implementazione di Haskell, l'interpolazione di una stringa è possibile grazie alla funzione `++` che concatena stringhe e alla funzione `show` che converte un tipo di dato in una stringa.

Vedi anche:
- [Funzioni di formattazione di stringhe in Haskell](https://www.haskell.org/hoogle/?hoogle=string+formatting)
- [AWK](https://www.gnu.org/software/gawk/)
- [Perl](https://www.perl.org/)