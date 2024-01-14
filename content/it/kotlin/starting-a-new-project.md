---
title:                "Kotlin: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Molte persone iniziano nuovi progetti in Kotlin perché è un linguaggio di programmazione moderno e potente, in grado di soddisfare le esigenze di una varietà di progetti, dall'app development alla web development.

## Come fare

Per iniziare un nuovo progetto in Kotlin, è necessario avere Java installato sul proprio computer. Una volta installato Java, è possibile procedere con l'installazione di IntelliJ, un'IDE (Integrated Development Environment) che supporta il linguaggio Kotlin.

Dopo aver installato IntelliJ, è possibile creare un nuovo progetto Kotlin selezionando "File" > "New" > "Project". Si aprirà una finestra in cui potrai scegliere il nome del progetto, la posizione in cui salvare i file e la versione di Kotlin da utilizzare.

Una volta creato il progetto, è possibile iniziare a scrivere il codice. Di seguito sono riportati alcuni esempi di codice Kotlin con il loro output.

```Kotlin
// Dichiarazione di una variabile
var numero = 10
println("Il numero è $numero")

// Output: Il numero è 10
```

```Kotlin
// Dichiarazione di una funzione
fun somma(a: Int, b: Int) {
    val ris = a + b
    println("Il risultato è $ris")
}

// Chiamata della funzione
somma(5, 3)

// Output: Il risultato è 8
```

```Kotlin
// Dichiarazione di una classe
class Persona(val nome: String, val cognome: String) {
    fun saluta() {
        println("Ciao, sono $nome $cognome")
    }
}

// Creazione di un'istanza della classe
val persona = Persona("Mario", "Rossi")
persona.saluta()

// Output: Ciao, sono Mario Rossi
```

È possibile vedere altri esempi di codice Kotlin sul sito ufficiale della documentazione: https://kotlinlang.org/docs/tutorials/getting-started.html

## Approfondimento

Oltre alle funzionalità di base, Kotlin offre molte altre caratteristiche interessanti come la sintassi concisa e l'immunità a Null Pointer Exceptions grazie al concetto di nullable e non-nullable types. Inoltre, Kotlin è completamente compatibile con Java, quindi è possibile utilizzare librerie Java esistenti all'interno di un progetto Kotlin.

Un buon modo per imparare Kotlin è partecipare a progetti open-source su GitHub, dove è possibile vedere come viene utilizzato in modo più approfondito e imparare dai contributi degli altri sviluppatori.

## Vedi anche

- Kotlin ufficiale: https://kotlinlang.org/
- Documentazione ufficiale: https://kotlinlang.org/docs/home.html
- GitHub: https://github.com/JetBrains/kotlin