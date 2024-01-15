---
title:                "Stampa della produzione di debug"
html_title:           "Swift: Stampa della produzione di debug"
simple_title:         "Stampa della produzione di debug"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché
Puoi chiederti perché dovresti preoccuparti di stampare output di debug nel tuo codice Swift. La risposta è semplice: la stampa di output di debug può aiutarti a identificare e risolvere errori nel tuo codice e a capire meglio cosa sta succedendo durante l'esecuzione del programma.

## Come fare
Per stampare output di debug nel tuo codice Swift, puoi utilizzare la funzione print(). Questa funzione accetta un numero variabile di parametri e li stampa sulla console. Di seguito un esempio di codice e il relativo output:

```Swift
let num1 = 10
let num2 = 5

print("La somma di num1 e num2 è \(num1 + num2)")
// Output: La somma di num1 e num2 è 15
```

Puoi anche utilizzare l'interpolazione delle stringhe per stampare il valore di una variabile all'interno di una stringa:

```Swift
let name = "Mario"
print("Ciao, mi chiamo \(name)")
// Output: Ciao, mi chiamo Mario 
```

## Approfondimenti
Oltre alla funzione print(), esistono altre due modalità per stampare output di debug nel tuo codice Swift: NSLog e la classica println(). Entrambe le funzioni funzionano in modo simile a print(), ma hanno alcune differenze nel formato di output. Puoi saperne di più sulle differenze tra queste funzioni e scegliere quella più adatta alle tue esigenze. Inoltre, è importante evitare di stampare troppi output di debug, in quanto potrebbe rallentare l'esecuzione del programma.

## Vedi anche
- [Guida introduttiva al debugging in Swift](https://www.raywenderlich.com/7738345-debugging-in-swift-getting-started)
- [Documentazione ufficiale di Swift su debug output](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)
- [Altro articolo su come utilizzare print() per il debugging in Swift](https://www.hackingwithswift.com/books/ios-swiftui/why-does-swift-have-a-built-in-logging-system)