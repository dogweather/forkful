---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

La conversione di una stringa in lettere minuscole è un'operazione comune nella programmazione. Questo perché ci permette di gestire l'input dell'utente in maniera più semplice e prevedibile, scongiurando qualsiasi errore dovuto a differenze di capitalizzazione.

## Come fare:

Ecco un semplice esempio di come convertire una stringa in lettere minuscole in Swift:

```Swift
let testo = "Ciao Mondo"
let testoMinuscolo = testo.lowercased()
print(testoMinuscolo)
// Output: "ciao mondo"
```
Questo codice converte il testo "Ciao Mondo" in "ciao mondo", quindi lo stampa.

## Approfondimento

- Contesto storico: Funzioni per convertire stringhe in lettere minuscole esistono da quando le prime librerie di manipolazione delle stringhe sono state create. Queste funzioni sono pensate per facilitare le operazioni su testo e per garantire coerenza nel trattamento dei dati.

- Alternative: Swift fornisce altri metodi per gestire le stringhe, come `uppercased()` per convertire la stringa in lettere maiuscole, o `capitalized` per capitalizzare ogni parola nel testo.

- Dettagli di implementazione: La funzione `lowercased()` di Swift non modifica la stringa originale (gli oggetti di tipo String in Swift sono immutabili). Invece, crea una nuova stringa con le lettere convertite in minuscolo.

## Vedi Anche:

- Documentazione di Swift: [String](https://developer.apple.com/documentation/swift/string)
- Swift Standard Library: [lowercased()](https://developer.apple.com/documentation/swift/string/2997127-lowercased)