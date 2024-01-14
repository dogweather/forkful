---
title:                "Swift: Stampa della rappresentazione debug"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un'importante pratica di programmazione che aiuta a identificare e risolvere rapidamente errori nel codice. Conoscere come stampare l'output di debug può aiutare a migliorare la qualità e l'efficienza del tuo codice.

## Come fare

Per stampare l'output di debug in Swift, è possibile utilizzare la funzione `print()`. Ad esempio, se si vuole stampare una variabile `x` con un messaggio di testo aggiuntivo, si può scrivere:

```Swift
print("Valore di x:", x)
```

Questo produrrà un output nel seguente formato:

```Console
Valore di x: 10
```

È anche possibile stampare più variabili o costanti in una singola riga, separandole con una virgola:

```Swift
let nome = "Marco"
let cognome = "Rossi"
print("Il tuo nome è", nome, "e il tuo cognome è", cognome)
```

Questo produrrà un output simile a questo:

```Console
Il tuo nome è Marco e il tuo cognome è Rossi
```

## Approfondimento

Ci sono alcune opzioni interessanti per la funzione `print()` che possono aiutare a migliorare l'output di debug. Alcune di queste includono:

- Utilizzare la funzione `dump()` invece di `print()` per stampare un output più dettagliato di una variabile, inclusi i suoi nomi di proprietà e i valori.
- Utilizzare la funzione `debugPrint()` per stampare una versione degli oggetti in modalità debug.
- Aggiungere un carattere di fine riga (`\n`) alla fine del messaggio stampato per andare a capo.

Inoltre, è possibile utilizzare `#if DEBUG` per condizionare l'esecuzione della funzione `print()` durante la fase di sviluppo, in modo che non venga eseguito quando l'applicazione viene compilata per la release finale.

## Vedi anche

- [Documentazione Apple su print()](https://developer.apple.com/documentation/swift/1541053-print)
- [Tutorial su print() di Hacking with Swift](https://www.hackingwithswift.com/example-code/language/how-to-dump-an-object-with-print)
- [Articolo su debugPrint() di NSHipster](https://nshipster.com/debugging/)