---
title:    "Swift: Maiuscolare una stringa"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'operazione comune nella programmazione Swift che può essere utile per formattare correttamente i dati o rendere più leggibile l'output.

## Come fare

Per capitalizzare una stringa, si può utilizzare il metodo `uppercased()` oppure `capitalized()` a seconda dell'effetto desiderato. Ecco un esempio:

```Swift
let stringa = "ciao a tutti"
print(stringa.uppercased()) // Output: CIAO A TUTTI
print(stringa.capitalized()) // Output: Ciao A Tutti
```

## Approfondimento

Il metodo `uppercased()` trasforma tutti i caratteri della stringa in maiuscolo, mentre `capitalized()` rende maiuscola solo la prima lettera di ogni parola. Entrambi i metodi sono sensibili alle impostazioni della lingua e possono produrre risultati diversi in base alla localizzazione del dispositivo.

## Vedi anche

Per ulteriori informazioni sulla manipolazione delle stringhe in Swift, si consiglia di consultare i seguenti link:

- [Documentazione ufficiale di Swift su stringhe](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial su Hacking with Swift](https://www.hackingwithswift.com/sixty/5/8/uppercase-strings-with-uppercased-and-capitalized) (in inglese)