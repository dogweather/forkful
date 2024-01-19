---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cosa e Perche?
Nella programmazione, "convertire una stringa in minuscolo" significa trasformare tutti i caratteri maiuscoli di una stringa in minuscoli. Questo è comune per normalizzare i dati di input, facendo in modo che il confronto delle stringhe sia insensibile al maiuscolo e minuscolo.

## Come fare:
In Kotlin, possiamo utilizzare il metodo `toLowerCase()` per convertire una stringa in minuscolo. Ecco un esempio semplice,

```Kotlin
fun main() {
    val str = "CIAO MONDO"
    val lowerCaseStr = str.toLowerCase()
    println(lowerCaseStr)
}
```
L'output sarà:

```Kotlin
ciao mondo
```

## Approfondimento:
- Contesto storico: l'operazione di conversione in minuscolo è presente da quando sono stati introdotti i linguaggi di programmazione. Ha le sue radici nel desiderio degli sviluppatori di rendere i loro programmi più tolleranti agli errori umani.
- Alternative: Un'altra funzione di Kotlin che può aiutare nello stesso contesto è `equals(str, ignoreCase = true)`. Ignora la distinzione tra maiuscole e minuscole durante la comparazione delle stringhe.
- Dettagli di implementazione: `toLowerCase()` in Kotlin utilizza le regole del Locale predefinite del sistema dove il programma viene eseguito. Se vuoi un comportamento coerente attraverso diverse località, dovresti usare `toLowerCase(Locale.ROOT)`.

```Kotlin
fun main() {
    val str = "CIAO MONDO"
    val lowerCaseStr = str.toLowerCase(Locale.ROOT)
    println(lowerCaseStr)
}
```
L'output sarà lo stesso:

```Kotlin
ciao mondo
```

## Vedi anche:
- [toUpperCase in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)

Ricorda, convertire una stringa in minuscolo può essere molto utile quando stai lavorando con un caso insensibile alle stringhe. Buona codifica!