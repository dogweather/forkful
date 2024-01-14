---
title:    "Swift: Estrazione di sottostringhe"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe è un'operazione fondamentale nella programmazione Swift che può aiutare a manipolare e gestire efficacemente le stringhe. Con questo articolo, scopriremo come estrarre facilmente le sottostringhe utilizzando esempi di codice.

## Come

Per estrarre una sottostringa da una stringa, possiamo utilizzare il metodo `prefix` o `suffix` di Swift. Ecco un esempio di codice utilizzando il metodo `prefix` per estrarre le prime tre lettere di una parola:

```
let parola = "ciao"
let sottostringa = parola.prefix(3)
print(sottostringa)
```

Questo codice produrrà l'output "cia" poiché il metodo `prefix` restituisce i primi tre caratteri della stringa originale.

Possiamo anche specificare un indice come argomento del metodo `prefix` per estrarre una sottostringa dall'inizio fino a quel determinato indice. Ad esempio:

```
let parola = "Swift Programming"
let sottostringa = parola.prefix(5)
print(sottostringa)
```

Questo codice produrrà l'output "Swift".

Analogamente, possiamo utilizzare il metodo `suffix` per estrarre una sottostringa dalla fine di una stringa. Ad esempio:

```
let parola = "ciao"
let sottostringa = parola.suffix(2)
print(sottostringa)
```

Questo codice produrrà l'output "ao".

## Approfondimento

I metodi `prefix` e `suffix` sono solo due dei molti modi per estrarre sottostringhe in Swift. Possiamo anche utilizzare il metodo `substring` o l'operatore di slicing `[]` per ottenere sottostringhe da una stringa. Inoltre, possiamo utilizzare gli indici degli elementi per estrarre sottostringhe in modo più preciso. Ad esempio:

```
let parola = "ciao"
let index = parola.index(parola.startIndex, offsetBy: 2)
let sottostringa = parola.prefix(upTo: index)
print(sottostringa)
```

Questo codice produrrà l'output "ci" poiché stiamo estraendo i primi due caratteri, utilizzando l'indice dell'elemento "o".

Utilizzando questi metodi e approcci, possiamo facilmente manipolare e gestire le stringhe in Swift.

## Vedi anche

- Documentazione Swift sulle sottostringhe: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID283
- Tutorial di Ray Wenderlich su come gestire le stringhe in Swift: https://www.raywenderlich.com/3571-swift-tutorial-part-2-strings-characters-and-arrays-in-swift