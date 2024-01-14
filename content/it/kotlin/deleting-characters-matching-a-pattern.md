---
title:    "Kotlin: Cancellare i caratteri corrispondenti a un modello"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano a dover manipolare stringhe di testo che contengono caratteri indesiderati o non necessari. Uno scenario comune potrebbe essere la pulizia dei dati di un file di testo importato in un'applicazione. In questi casi, è utile sapere come eliminare i caratteri che corrispondono a un certo pattern.

## Come Fare

```Kotlin
// Creiamo una stringa con un esempio di caratteri indesiderati
val testString = "Ciao! ##Come stai?## Vuoi imparare a programmare in Kotlin?"

// Usiamo la funzione replace() per sostituire i caratteri specificati con una stringa vuota
val cleanString = testString.replace(Regex("[#?]"), "")

// Stampa il risultato
println(cleanString)

// Output: Ciao! Come stai? Vuoi imparare a programmare in Kotlin?
```

Nell'esempio sopra, abbiamo utilizzato la funzione `replace()` di Kotlin per sostituire i caratteri "#" e "?" con una stringa vuota, eliminandoli dalla stringa originale. La funzione accetta un oggetto Regex (espressione regolare) come primo parametro, che ci permette di specificare in modo più preciso quali caratteri vogliamo eliminare. 

## Approfondimento

L'uso della funzione `replace()` è solo una delle tante possibilità per eliminare caratteri che corrispondono a un pattern. Alcune altre opzioni potrebbero essere l'utilizzo della funzione `filter()` o il parsing della stringa in un array e l'eliminazione dei singoli caratteri indesiderati prima di ricostruire la stringa. Inoltre, l'uso delle espressioni regolari ci permette di avere una maggiore flessibilità nel definire il pattern dei caratteri da eliminare. 

## Vedi Anche

- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/reference/strings.html#string-templates-and-regular-expressions)
- [Tutorial su espressioni regolari in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm)
- [Utilizzo delle funzioni `replace()` e `filter()` in Kotlin](https://medium.com/@agrawalsuneeti/character-patterns-in-kotlin-e978f3a66f1b)