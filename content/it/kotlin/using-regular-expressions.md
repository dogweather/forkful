---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Le espressioni regolari (regex) permettono di riconoscere pattern di testo. Sono uno strumento potente per la validazione, l'estrazione e la manipolazione di stringhe in modo efficiente.

## Come fare:
Esempi di codice con output.

```kotlin
fun main() {
    val regex = Regex("[a-z]+")
    val matchResult = regex.find("ciao mondo123") 
    println(matchResult?.value)  // Output: ciao

    val regexReplace = Regex("\\s+")
    val replaced = regexReplace.replace("Spazi    multipli", " ")
    println(replaced)  // Output: Spazi multipli

    val regexSplit = Regex(",")
    val splitList = regexSplit.split("mele,arance,banane")
    println(splitList)  // Output: [mele, arance, banane]
}
```

## Nel Dettaglio
Le regex risalgono agli anni '50 e furono introdotte da Stephen Kleene. Alternative alle regex includono parser e tokenizzatori specifici, ma nessuno è altrettanto versatile. Le regex in Kotlin sono implementate tramite la classe `Regex` e supportano la maggior parte dei pattern standard.

## Vedi Anche
- [Documentazione ufficiale Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Lezioni su espressioni regolari su RegexOne](https://regexone.com/)