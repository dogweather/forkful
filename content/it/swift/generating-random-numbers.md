---
title:                "Generazione di numeri casuali"
html_title:           "Swift: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

Cosa & Perché?:

Generare numeri casuali è una funzionalità importante per i programmatori, in quanto consente di creare un flusso casuale di dati in un programma. Questo può essere utile per simulazioni, giochi, criptografia e altro ancora.

Come Fare:

Ecco un esempio di codice Swift per generare un numero casuale tra 1 e 10 e stamparlo a schermo:

```Swift
let randomNumber = Int.random(in: 1...10)
print("Numero casuale: \(randomNumber)")
```

Output:
Numero casuale: 7

Vediamo più in dettaglio come funziona il codice:
- Utilizziamo il metodo `random(in:)` della classe `Int` per generare un numero casuale all'interno di un range specificato.
- Il range `[1...10]` indica che il numero casuale sarà compreso tra 1 e 10, inclusi entrambi.
- Infine, utilizziamo il metodo `print()` per stampare il numero casuale a schermo, concatenandolo con una stringa utilizzando l'operatore `\()`.

Per ottenere un numero casuale tra 0 e 1, possiamo utilizzare il metodo `Double.random()`:

```Swift
let randomDouble = Double.random()
print("Numero casuale tra 0 e 1: \(randomDouble)")
```

Output:
Numero casuale tra 0 e 1: 0.865149420820621

Deep Dive:

La generazione di numeri casuali è una funzionalità comune nei linguaggi di programmazione e risale all'inizio della computer science. Prima dell'introduzione dei metodi nativi per la generazione di numeri casuali, i programmatori dovevano utilizzare algoritmi matematici per ottenere risultati casuali.

Oltre ai metodi nativi, esistono anche librerie esterne per la generazione di numeri casuali, come ad esempio la libreria `GameKit` di Apple. È importante ricordare che i numeri casuali generati dai metodi nativi non sono veramente "random", ma sembrano casuali agli occhi degli utilizzatori.

Vedi Anche:

Per ulteriori informazioni sulla generazione di numeri casuali in Swift, puoi consultare la documentazione ufficiale di Apple: https://developer.apple.com/documentation/swift/using_random_numbers_in_your_code.

Puoi anche trovare ulteriori esempi di codice e tutorial su siti di sviluppatori come Hacking with Swift: https://www.hackingwithswift.com/example-code/system/how-to-generate-random-numbers-using-randomonom-in-swift.