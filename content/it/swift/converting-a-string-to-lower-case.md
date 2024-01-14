---
title:    "Swift: Trasformare una stringa in minuscolo"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui un programmatore potrebbe voler convertire una stringa in minuscolo. Ad esempio, potrebbe essere necessario confrontare due stringhe senza considerare le differenze tra maiuscole e minuscole oppure si potrebbe voler ottenere una stringa in un formato uniforme per utilizzi futuri. In generale, la conversione di una stringa in minuscolo può semplificare molti processi di elaborazione dei dati.

## Come Fare

Per convertire una stringa in minuscolo in Swift, possiamo utilizzare il metodo `lowercased()` sul valore della stringa. Vediamo un esempio pratico:

```Swift
let stringa = "Hello World"
let stringaInMinuscolo = stringa.lowercased()
print(stringaInMinuscolo) //stamperà: hello world
```

Come possiamo vedere, il metodo `lowercased()` ci restituisce una nuova stringa con tutti i caratteri in minuscolo. Questo metodo è disponibile per tutti i tipi di stringhe e può essere facilmente utilizzato nei nostri progetti Swift.

## Approfondimento

In Swift, la conversione di una stringa in minuscolo si basa sulle specifiche Unicode e tiene conto delle possibili varianti di maiuscole e minuscole di ogni singolo carattere. Inoltre, possiamo specificare esplicitamente la localizzazione in cui vogliamo effettuare la conversione, garantendo così una maggiore precisione nel nostro codice.

## Vedi Anche

- [Documentazione ufficiale di Apple su lowercased()](https://developer.apple.com/documentation/swift/string/2427836-lowercased)
- [Tutorial su come manipolare le stringhe in Swift](https://www.ralfebert.de/ios-examples/string/basics/)
- [Ulteriori informazioni sull'Unicode nelle stringhe di Swift](https://www.hackingwithswift.com/example-code/strings/swifts-strings-and-unicode)