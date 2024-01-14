---
title:    "Swift: Stampa dell'output di debug"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un'importante abilità per ogni programmatore Swift. Ci permette di verificare il funzionamento del nostro codice e di individuare eventuali errori o problemi. Inoltre, l'output di debug può aiutare a comprendere meglio il flusso di esecuzione del programma.

## Come Fare

Per stampare l'output di debug in Swift, possiamo utilizzare la funzione "print". Questa funzione accetta un numero variabile di parametri di qualsiasi tipo e li stampa sulla console. Vediamo un esempio:

```Swift
let nome = "Mario"
let eta = 30
print("Ciao, mi chiamo \(nome) e ho \(eta) anni.")
```

L'output di questo codice sarà:

```
Ciao, mi chiamo Mario e ho 30 anni.
```

Possiamo anche utilizzare la funzione "debugPrint" per stampare un oggetto in modo più dettagliato, includendo anche il tipo di dati e altre informazioni utili per il debugging.

```Swift
let listaSpesa = ["pane", "latte", "frutta"]
debugPrint(listaSpesa)
```

L'output sarà simile a questo:

```
["pane", "latte", "frutta"]
```

## Approfondimento

Stampare l'output di debug è particolarmente utile quando si lavora con strutture dati complesse come array, dizionari o oggetti personalizzati. In questi casi, utilizzare la funzione "dump" può essere più efficace. Questa funzione ci mostra a schermo una rappresentazione più dettagliata dell'oggetto, compresi i suoi valori e le sue proprietà.

```Swift
struct Persona {
    var nome: String
    var eta: Int
}

let persone = [
    Persona(nome: "Luca", eta: 25),
    Persona(nome: "Giulia", eta: 32)
]

dump(persone)
```

L'output sarà:

```
▿ 2 elements
  ▿ Person
    - name: "Luca"
    - age: 25
  ▿ Person
    - name: "Giulia"
    - age: 32
```

## Vedi Anche

- [Documentazione ufficiale di Swift su debugging](https://docs.swift.org/swift-book/LanguageGuide/PrintingAndDebugging.html)
- [Articolo su stampa di output di debug in Swift](https://medium.com/ios-os-x-development/basic-debugging-with-print-and-the-debug-console-in-swift-2b2ed26505c9) (in inglese)