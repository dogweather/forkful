---
title:                "Generazione di numeri casuali"
date:                  2024-01-27T20:35:25.921645-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"

category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

La generazione di numeri casuali nella programmazione riguarda la creazione di valori numerici non deterministici o imprevedibili. I programmatori utilizzano i numeri casuali per una varietà di motivi, come per simulare l'imprevedibilità nei giochi, selezionare campioni casuali da insiemi di dati o per scopi crittografici.

## Come fare:

Swift offre un modo diretto per generare numeri casuali attraverso la sua libreria standard. Ecco come si fa per diversi tipi numerici:

```Swift
// Genera un intero casuale tra 0 e Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Genera un numero in virgola mobile casuale tra 0.0 e 1.0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Genera un valore Bool casuale
let randomBool = Bool.random()
print(randomBool)
```

L'output dei campioni potrebbe variare perché, dopotutto, stiamo trattando con la casualità. Eseguendo il codice più volte si otterranno numeri e valori booleani diversi.

## Approfondimento

L'approccio di Swift alla generazione di numeri casuali si basa su un generatore di numeri pseudo-casuali (PRNG) robusto ed efficiente. Prima di Swift 4.2, gli sviluppatori si affidavano a librerie esterne o alle capacità della piattaforma sottostante, il che poteva portare a incongruenze tra diverse piattaforme e ambienti. Con l'introduzione di API native in Swift 4.2, generare numeri casuali è diventato più semplice e più consistente, indipendentemente dalla piattaforma sottostante.

Tuttavia, è fondamentale comprendere che il generatore di numeri casuali standard in Swift non è adatto a scopi crittografici. Per la crittografia, gli sviluppatori dovrebbero utilizzare il framework `Security` sulle piattaforme Apple, che fornisce accesso a byte casuali sicuri dal punto di vista crittografico. Al momento del mio ultimo aggiornamento, Swift non include un generatore di numeri casuali crittografici cross-platform nella sua libreria standard, costringendo gli sviluppatori a cercare librerie di terze parti per tali necessità su piattaforme non-Apple.

Nel regno del calcolo scientifico o in situazioni che richiedono una sequenza deterministica di numeri pseudo-casuali (in cui la sequenza può essere riprodotta esattamente), la generazione di numeri casuali di Swift potrebbe non essere la scelta migliore senza la possibilità di inizializzare il generatore. In questi casi, spesso si ricorre a librerie e algoritmi specializzati per soddisfare queste esigenze precise.
