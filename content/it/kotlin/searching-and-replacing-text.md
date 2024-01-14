---
title:                "Kotlin: Ricerca e sostituzione di testo."
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Molti sviluppatori saranno d'accordo sul fatto che il testo è una parte fondamentale di qualsiasi programma. Ciò significa che, a volte, può essere necessario cercare e sostituire parti di un testo. Fortunatamente, Kotlin fornisce delle funzionalità che rendono questo processo semplice e veloce.

## Come Fare

Per cercare e sostituire un testo in Kotlin, puoi utilizzare il metodo `replace()` ed il metodo `replaceAll()`. Il metodo `replace()` sostituisce solo la prima occorrenza della stringa, mentre `replaceAll()` sostituisce tutte le occorrenze. Vediamo un esempio:

```Kotlin
val text = "Questo è un testo di esempio."
val nuovoTesto = text.replace("testo", "codice")
val nuovoTesto2 = text.replaceAll("testo", "codice")

println(nuovoTesto) // Output: Questo è un codice di esempio.
println(nuovoTesto2) // Output: Questo è un codice di esempio.
```

Come puoi vedere, il metodo `replace()` ha sostituito solo la prima occorrenza della parola "testo", mentre il metodo `replaceAll()` ha sostituito tutte le occorrenze.

Un'altra opzione è utilizzare l'espressione regolare `Regex` per eseguire una ricerca più avanzata e sostituire il testo. Vediamo un esempio:

```Kotlin
val text = "Questo è un testo di esempio 123."
val nuovoTesto = text.replace(Regex("[0-9]"), "X")

println(nuovoTesto) // Output: Questo è un testo di esempio XXX.
```

L'espressione regolare `[0-9]` indica di cercare tutte le cifre da 0 a 9 e sostituirle con la lettera "X".

## Approfondimento

Kotlin fornisce anche altre funzioni utili per la ricerca e la sostituzione di testo, come ad esempio `replaceRange()` per sostituire solo una parte specifica di una stringa o `buildString()` per costruire una nuova stringa durante il processo di sostituzione.

Inoltre, è possibile utilizzare `StringTemplate` per eseguire sostituzioni di variabili all'interno di una stringa. Ad esempio:

```Kotlin
val nome = "Mario"
val age = 30

val profilo = "Il mio nome è \${nome} e ho \${age} anni."

println(profilo) // Output: Il mio nome è Mario e ho 30 anni.
```

## Vedi Anche

- [Documentazione ufficiale su Kotlin per la sostituzione di testo](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Tutorial su espressioni regolari in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm)
- [Esempi pratici di utilizzo di StringTemplate in Kotlin](https://try.kotlinlang.org/#/Examples/Strings/String%20Templates/String%20Templates.kt)