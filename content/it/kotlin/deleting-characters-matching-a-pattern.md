---
title:                "Kotlin: Cancellare caratteri corrispondenti a un determinato pattern"
simple_title:         "Cancellare caratteri corrispondenti a un determinato pattern"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler cancellare i caratteri che corrispondono ad un determinato modello nel tuo programma Kotlin. Ad esempio, potresti voler pulire un input utente per garantire la sicurezza del tuo programma o potresti dover formattare una stringa prima di salvarla in un database.

## Come fare

Per cancellare i caratteri che corrispondono ad un modello, puoi utilizzare la funzione `replace` nella classe `String` di Kotlin. Questa funzione accetta due argomenti: il primo è il modello dei caratteri da sostituire e il secondo è la stringa di sostituzione.

```Kotlin
val str = "Questo è un esempio di stringa con dei numeri 12345 e delle lettere abcdef"
val cleanedStr = str.replace(Regex("[0-9]+"), "") // Rimuove tutti i numeri dalla stringa
println(cleanedStr)

// Output: Questo è un esempio di stringa con dei numeri e delle lettere abcdef
```

In questo esempio, utilizziamo la classe `Regex` per creare un modello che rappresenta qualsiasi sequenza di numeri. Questo modello verrà quindi usato nella funzione `replace` per rimuovere tutti i numeri dalla stringa di input.

## Approfondimento

Se vuoi saperne di più sulla cancellazione dei caratteri corrispondenti ad un pattern, ti consiglio di esplorare la funzione `replace` nella documentazione ufficiale di Kotlin. Inoltre, puoi sperimentare con altri tipi di modelli e stringhe di sostituzione per ottenere risultati diversi. Ricorda sempre di gestire eventuali errori che possono verificarsi durante la cancellazione dei caratteri.

## Vedi anche

- [Documentazione ufficiale di Kotlin su funzione replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Tutorial su come utilizzare la classe Regex in Kotlin](https://www.baeldung.com/kotlin/regex)