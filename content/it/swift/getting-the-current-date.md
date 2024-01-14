---
title:                "Swift: Ottenere la data corrente"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Oggi parliamo di un concetto fondamentale nella programmazione di Swift: ottenere la data corrente. Perché dovresti interessarti a questa funzione? Semplice, è essenziale per qualsiasi applicazione che deve gestire il tempo.

## Come fare

Per ottenere la data corrente in Swift, possiamo utilizzare la classe `Date` e il suo inizializzatore `init()` che prende in input l'ora attuale. Possiamo poi usare un oggetto `DateFormatter` per formattare la data nel modo desiderato. Vediamolo in azione:

```Swift
let dataCorrente = Date()
let formattatore = DateFormatter()

formattatore.dateFormat = "dd/MM/yyyy" // Impostiamo il formato desiderato
let dataFormattata = formattatore.string(from: dataCorrente) // Convertiamo la data in una stringa formattata

print(dataFormattata) // Stampa ad esempio: 01/01/2021
```

In questo esempio, abbiamo ottenuto la data corrente e l'abbiamo formattata nel formato giorno/mese/anno. Possiamo modificare il formato semplicemente cambiando la stringa del `dateFormat` dell'oggetto `DateFormatter`.

## Approfondimento

Ora che abbiamo visto come ottenere la data corrente, è importante capire come funziona il concetto di data in Swift. Date in Swift sono rappresentate da un valore di tempo assoluto e non dipendono dal fuso orario o dal calendario dell'utente. Quindi è importante assicurarsi di gestire correttamente la conversione in fuso orario locale e il formato della data per l'utente.

Una data in Swift può anche essere rappresentata come componente di data, come anno, mese, giorno, ora, minuti e secondi. Possiamo accedere a questi componenti tramite la classe `Calendar` utilizzando il metodo `component(_:from:)`.

Per maggiori informazioni e approfondimenti sulla gestione delle date in Swift, consulta la documentazione ufficiale di Apple e gli altri articoli sul nostro blog.

## Vedi anche

- [Documentazione ufficiale Apple su Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial di Ray Wenderlich su Date in Swift](https://www.raywenderlich.com/7181016-dates-and-times-in-swift-getting-started)
- [Altro articolo sul nostro blog: Gestire i fusi orari in Swift](https://linkalaltroarticolo.it)