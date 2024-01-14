---
title:    "Kotlin: Ricerca e sostituzione di testo"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Sarete in grado di risparmiare tempo e sforzi modificando i vostri testi con facilità, grazie alla capacità di cercare e sostituire in modo efficiente il testo in codice Kotlin.

## Come Fare

Nel linguaggio di programmazione Kotlin, è possibile utilizzare la funzione "replace" per sostituire una stringa con un'altra. Ecco un esempio di codice:

```Kotlin
var testo = "Ciao amici!"
var risultato = testo.replace("amici", "ragazzi")
println(risultato) // Stampa "Ciao ragazzi!"
```

In questo esempio, abbiamo sostituito la parola "amici" con "ragazzi" all'interno della stringa "Ciao amici!". Possiamo anche utilizzare espressioni regolari per sostituire del testo specifico. Ecco un altro esempio:

```Kotlin
var testo = "Abbiamo bisogno di 10 mele."
var risultato = testo.replace(Regex("[0-9]+"), "5")
println(risultato) // Stampa "Abbiamo bisogno di 5 mele."
```

In questo caso, abbiamo utilizzato un'espressione regolare per trovare qualsiasi numero all'interno della stringa e sostituirlo con il numero 5.

## Approfondimento

Oltre alla semplice sostituzione di stringhe, Kotlin offre anche la possibilità di utilizzare l'operatore di assegnazione "=". Questo operatore sostituirà la stringa originale solo se è presente nella stringa iniziale. Ecco un esempio:

```Kotlin
var testo = "Ciao a tutti!"
testo = testo.replace("salve", "ciao")
println(testo) // Stampa "Ciao a tutti!", perché "salve" non era presente nella stringa originale.
```

Oltre alla funzione "replace" e all'operatore "=", Kotlin offre anche altre funzioni utili per la ricerca e la sostituzione di stringhe, come "replaceFirst" e "replaceAll".

## Vedi anche

- Documentazione ufficiale di Kotlin sulle funzioni di sostituzione stringa: [sostituzione stringa](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- Tutorial su come utilizzare le espressioni regolari in Kotlin: [espressioni regolari in Kotlin](https://www.tutorialkart.com/kotlin/regular-expression-in-kotlin/)