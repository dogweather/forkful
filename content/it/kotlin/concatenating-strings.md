---
title:                "Concatenazione di stringhe"
html_title:           "Kotlin: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Perché qualcuno dovrebbe voler concatenare le stringhe? Perché è un'operazione fondamentale in molti casi di utilizzo del linguaggio di programmazione Kotlin. Ad esempio, possono essere necessarie per la gestione delle stringhe di input utente, per la formattazione dei dati e per la creazione di output nei programmi.

## Come Fare
La concatenazione delle stringhe in Kotlin è semplice e intuitiva. Per unirle insieme, è possibile utilizzare il simbolo "+" o il metodo "plus". Ad esempio:

```Kotlin
val string1 = "Ciao"
val string2 = "mondo"
val string3 = string1 + " " + string2
println(string3) // Output: Ciao mondo
```

Si può anche utilizzare la funzione di formattazione "format" per combinare stringhe e variabili. Ad esempio:

```Kotlin
val nome = "Carlo"
val cognome = "Rossi"
val stringa = "Il mio nome è %s %s".format(nome, cognome)
println(stringa) // Output: Il mio nome è Carlo Rossi
```

## Approfondimento
Quando si concatenano molte stringhe, è importante tenere conto delle prestazioni del proprio codice. In Kotlin, alloca sempre una nuova stringa quando si utilizza il simbolo "+" per la concatenazione. Questo può essere inefficiente se si lavora con grandi quantità di dati. In alternativa, è possibile utilizzare la classe "StringBuilder", che è progettata appositamente per l'aggiunta e la modifica di grandi quantità di testo in modo efficiente.

Ad esempio:

```Kotlin
val sb = StringBuilder()
sb.append("Questo")
sb.append(" è ")
sb.append("un esempio")
println(sb.toString()) // Output: Questo è un esempio
```

## Vedi Anche
- [Documentazione ufficiale di Kotlin su Stringhe](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial su Kotlin stringhe](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)
- [Video tutorial su Kotlin stringhe](https://www.youtube.com/watch?v=3C5sJt4kjsE)