---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stringhe Interpolate in Swift - Snellimento dell'Input & Output 

## Cos'è & Perché?

L'interpolazione di stringhe è un modo per combinare variabili e costanti all'interno di una stringa. La ragione principale per utilizzarlo è per comporre stringhe di modo dinamico e intuitivo.

## Come si fa:

In Swift, usiamo una sintassi specifica per interpolare una stringa. Dai un'occhiata qui sotto:

```Swift 
let nome = "Mario"
let mestiere = "programmatore"
let descrizione = "Il mio nome è \(nome) e sono un \(mestiere)."
print(descrizione)
```
Questa riga di codice produrrà l'output: 
```
Il mio nome è Mario e sono un programmatore.
``` 

## Approfondimento

L'interpolazione di stringhe non è un concetto nuovo e esiste da quando ci sono i linguaggi di programmazione. In Swift, tuttavia, è molto più potente che in molti altri linguaggi. Puoi, ad esempio, eseguire operazioni o chiamate a funzioni direttamente all'interno delle parentesi graffe.

Come alternativa in Swift, potresti concatenare le stringhe con l'operatore `+`, ma verrà meno leggibile e maneggiabile, specialmente con stringhe più complesse. 

In termini di implementazione, il compilatore Swift sostituisce le espressioni all'interno delle parentesi graffe (`\(...)`) con il risultato della loro valutazione. Questo significa che il codice all'interno delle parentesi viene effettivamente eseguito.

## Vedi anche:

- [Interpolazione di stringhe in Swift - Documentazione Ufficiale](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Swift String Interpolation Part 1 e Part 2 - Tutorial](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift)

Nota che Swift continua ad aggiornarsi, quindi verifica sempre che i tuoi metodi siano aggiornati con l'ultima versione!