---
title:    "Swift: Lettura degli argomenti della riga di comando"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se siete sviluppatori Swift, è probabile che abbiate sentito parlare di argomenti della riga di comando. Questi sono input che vengono passati al vostro programma quando viene eseguito da una riga di comando o da una shell. Ma perché dovreste imparare a leggere questi argomenti? Beh, ci sono molti casi in cui questo può essere utile, ad esempio quando si vuole fornire opzioni personalizzabili al vostro programma o quando si vuole passare dati esterni al programma.

## Come fare

In Swift, possiamo leggere gli argomenti della riga di comando utilizzando l'oggetto `CommandLine`, che rappresenta l'intera riga di comando. Esistono due metodi principali per accedere agli argomenti:

1. Usando gli indici degli argomenti per accedere a un argomento specifico
2. Iterando attraverso tutti gli argomenti utilizzando un ciclo `for`

Qui di seguito trovate un esempio di codice che mostra come leggere il primo argomento passato al programma:

```Swift
let firstArgument = CommandLine.arguments[1]
print("Il primo argomento è: \(firstArgument)")
```

Se si vuole iterare attraverso tutti gli argomenti, possiamo utilizzare il seguente codice:

```Swift
for (index, argument) in CommandLine.arguments.enumerated() {
    print("Argomento \(index): \(argument)")
}
```

Ecco un esempio di output di entrambi i metodi:

```shell
$ swift read_args.swift Hello World
Il primo argomento è: Hello
Argomento 0: /usr/bin/swift
Argomento 1: read_args.swift
Argomento 2: Hello
Argomento 3: World
```

## Approfondimento

Oltre ai metodi di base per leggere gli argomenti della riga di comando, ci sono alcune cose da tenere a mente quando si lavora con essi:

- Il primo argomento (indice 0) è sempre il percorso del programma stesso
- Gli spazi nei valori passati come argomenti vengono ignorati, a meno che non siano racchiusi tra virgolette `" "` o apostrofi `' '`
- Se si passano argomenti contenenti `=` (ad esempio `--key=value`), il segno `=` verrà ignorato e il valore verrà incluso nell'argomento

Inoltre, si possono utilizzare librerie di terze parti come `CommandLineKit` per semplificare la gestione degli argomenti della riga di comando in Swift.

## Vedi anche

- [Documentazione di Apple: CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [CommandLineKit su GitHub](https://github.com/jatoben/CommandLine)
- [Articolo su Swift by Sundell: Working with the command line in Swift](https://www.swiftbysundell.com/articles/working-with-the-command-line-in-swift/)