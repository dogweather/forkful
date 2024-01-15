---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Kotlin: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Ciao a tutti! Se stai leggendo questo articolo, probabilmente hai sentito parlare di *command line arguments* (argomenti della riga di comando) e sei interessato a saperne di più. Beh, sei nel posto giusto! Continua a leggere per scoprire come leggere e utilizzare i *command line arguments* nella tua programmazione Kotlin!

## Come fare

Iniziamo con un semplice esempio di codice per leggere un *command line argument* e stamparlo sulla console:

```
Kotlin
fun main(args: Array<String>) {

    // leggi il primo argomento dalla riga di comando
    val arg = args[0]

    // stampa l'argomento sulla console
    println("Il tuo command line argument è: $arg")
}
```

Se esegui questo codice con l'argomento "ciao" nella riga di comando, vedrai l'output "Il tuo command line argument è: ciao".

Ora, passiamo a un esempio più complesso che legge più di un argument e li utilizza in un calcolo matematico:

```
Kotlin
fun main(args: Array<String>) {

    // leggi il primo e il secondo argomento come numeri interi
    val num1 = args[0].toInt()
    val num2 = args[1].toInt()

    // esegui una semplice operazione matematica utilizzando gli argomenti
    val sum = num1 + num2

    // stampa il risultato sulla console
    println("La somma dei tuoi argomenti è: $sum")
}
```

Se esegui questo codice con gli argomenti "5 10", vedrai l'output "La somma dei tuoi argomenti è: 15".

## Approfondimento

Ora che hai visto qualche esempio pratico, forse hai ancora alcune domande su cosa siano esattamente i *command line arguments* e come funzionano. In breve, i *command line arguments* sono valori inseriti nella riga di comando quando si esegue un programma, che vengono poi letti e utilizzati dal programma stesso. Sono spesso utilizzati per fornire input personalizzato al programma ogni volta che viene eseguito.

In Kotlin, i *command line arguments* vengono passati al metodo `main()` come un array di stringhe (`Array<String>`). Puoi accedere a un singolo argomento utilizzando l'indice dell'array come abbiamo visto negli esempi di codice precedenti. Inoltre, puoi utilizzare il metodo `toInt()` per convertire una stringa in un numero intero, se necessario.

Ora che hai una comprensione più profonda dei *command line arguments*, puoi utilizzarli per personalizzare i tuoi programmi Kotlin in base alle tue esigenze specifiche.

## Vedi anche

- [Documentazione Kotlin: Argomenti della riga di comando](https://kotlinlang.org/docs/reference/command-line.html)
- [Tutorialspoint: Command Line Arguments in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_command_line_arguments.htm)
- [Kotlin By Example: Working with command line arguments in Kotlin](https://www.kotlinbyexample.org/working-with-command-line-arguments-in-kotlin/)