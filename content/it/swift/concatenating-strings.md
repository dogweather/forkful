---
title:                "Concatenazione di stringhe"
html_title:           "Swift: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Concatenare le stringhe in Swift significa unire due o più stringhe insieme per formare una nuova stringa più lunga. I programmatori spesso eseguono questa operazione per creare messaggi personalizzati, creare nomi univoci o semplicemente per modificare il contenuto di una stringa esistente.

## Come fare:
Il modo più semplice per concatenare le stringhe in Swift è utilizzando l'operatore "+". Esempio:

```Swift
let nome = "Marco"
let cognome = "Rossi"
let nomeCompleto = nome + " " + cognome
print(nomeCompleto)
```

Output:
```
Marco Rossi
```

## Approfondimento:
L'operatore "+" è stato introdotto nella prima versione di Swift, ed è l'opzione più comune per concatenare le stringhe. Tuttavia, esistono anche altri metodi, come l'utilizzo del metodo .appending(). Inoltre, è possibile concatenare anche stringhe con valori di altri tipi di dati, utilizzando il metodo .map().

## Vedi anche:
Per ulteriori informazioni su come gestire le stringhe in Swift, puoi consultare la documentazione ufficiale di Apple su [String and Character](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html).