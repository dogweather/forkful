---
title:                "Kotlin: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Convertire una stringa in minuscolo può essere utile quando si desidera uniformare il testo o confrontare due stringhe in modo case-insensitive.

## Come fare
Per convertire una stringa in minuscolo in Kotlin, si può utilizzare il metodo `toLowerCase()` della classe `String`. Di seguito è riportato un esempio di codice che mostra come utilizzare questo metodo:

```Kotlin
val stringa = "KOTLIN È DIVERTENTE!"
val lowerCase = stringa.toLowerCase()

println(lowerCase) // output: kotlin è divertente!
```
Nell'esempio sopra, viene creato un oggetto `String` con il valore "KOTLIN È DIVERTENTE!" e poi viene applicato il metodo `toLowerCase()` per convertirlo in minuscolo. Il risultato viene quindi stampato a schermo.

Un'altra opzione è utilizzare il metodo `lowercase()` della classe `StringBuilder` per modificare direttamente la stringa originale:

```Kotlin
var stringa = "MAIUSCOLO"
stringa = stringa.lowercase()

println(stringa) // output: maiuscolo
```

## Deep Dive
Un aspetto importante da tenere in considerazione durante la conversione di una stringa in minuscolo è la gestione degli accenti. Kotlin utilizza il sistema Unicode per rappresentare i caratteri, quindi è necessario usare il metodo `toLowerCase(Locale.getDefault())` per ottenere un risultato corretto in tutte le lingue.

In caso contrario, i caratteri accentati potrebbero essere convertiti in caratteri speciali come nel seguente esempio:

```Kotlin
val stringa = "CIAO ÀÈÌÒÙ"
val lowerCase = stringa.toLowerCase()

println(lowerCase) // output: ciao Ã Ã¨Ã¬Ã²Ã¹
```

Inoltre, è possibile utilizzare il metodo `toUpperCase()` per convertire una stringa in maiuscolo.

## Vedi anche
Se sei interessato a saperne di più su come gestire le stringhe in Kotlin, consulta i seguenti link:

- [Documentazione di Kotlin sui metodi per la manipolazione delle stringhe](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
- [Tutorial su come gestire le stringhe in Kotlin](https://www.tutorialkart.com/kotlin/strings/)