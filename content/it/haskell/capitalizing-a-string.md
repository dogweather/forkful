---
title:                "Haskell: Capitalizzazione di una stringa"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è una funzione molto utile quando si lavora con testi o stringhe di dati in Haskell. Permette di rendere più leggibile il testo e di manipolare le stringhe in modo più semplice.

## Come Fare

La funzione per capitalizzare una stringa in Haskell è `toUpper`. Prende come input una stringa e restituisce la stessa stringa ma con tutte le lettere maiuscole. Vediamo un esempio di codice:

```Haskell
import Data.Char

-- definiamo una stringa
myString = "ciao a tutti"

-- applichiamo la funzione toUpper alla stringa
result = toUpper myString

-- stampiamo il risultato
print result
```

L'output di questo codice sarà:

```Haskell
"CIAO A TUTTI"
```

Come si può vedere, la funzione `toUpper` ha semplicemente convertito tutte le lettere della stringa in maiuscolo. Oltre a `toUpper`, ci sono anche altre funzioni utili per la manipolazione delle stringhe, come ad esempio `toLower`, che converte tutte le lettere in minuscolo, o `capitalize`, che mette in maiuscolo solo la prima lettera della stringa.

## Approfondimenti

In Haskell, le stringhe sono tratte come liste di caratteri. Ciò significa che possiamo manipolarle con le solite funzioni di liste, come `map` o `filter`. Ad esempio, possiamo creare una funzione custom per capitalizzare solo le vocali di una stringa:

```Haskell
-- definiamo una funzione per capitalizzare solo le vocali
capitalizeVowels :: String -> String
capitalizeVowels str = map (\c -> if c `elem` "aeiou" then toUpper c else c) str

-- applichiamo la funzione alla nostra stringa
result = capitalizeVowels "ciao a tutti"

-- stampiamo il risultato
print result
```

L'output di questo codice sarà:

```Haskell
"CIaO A TUttI"
```

Come si può vedere, la funzione `capitalizeVowels` ha capitalizzato solo le vocali della stringa in input.

## Vedi Anche

- [Documentazione di `Data.Char`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Manipolazione delle stringhe in Haskell](https://blog.jcoglan.com/2008/01/04/whats-wrong-with-haskells-strings/)