---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "Kotlin: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui qualcuno potrebbe voler cancellare i caratteri che corrispondono a un certo modello all'interno di un testo. Potresti voler pulire dei dati in un file di testo, rimuovere informazioni sensibili da un documento o semplicemente semplificare il tuo codice.

## Come fare

Per eliminare i caratteri che corrispondono a un certo modello, possiamo utilizzare il metodo `replace` della classe `StringBuilder` di Kotlin.

```kotlin
val text = "Questa è una stringa di testo con alcune parole censurate come **** e ****"
val censoredText = StringBuilder(text).replace("[*]+".toRegex(), "")
println(censoredText)
```

L'output sarà:

```
Questa è una stringa di testo con alcune parole censurate come e
```

In questo esempio, abbiamo utilizzato un'espressione regolare per cercare e sostituire tutti i gruppi di asterischi con una stringa vuota.

## Approfondimento

Kotlin fornisce diversi metodi per gestire l'eliminazione di caratteri basati su un modello. Per esempio, il metodo `deleteRange` della classe `StringBuilder`ci permette di eliminare una porzione di testo specificata tramite indice.

```kotlin
val text = "Questa è una stringa di testo"
val newText = StringBuilder(text).deleteRange(0, 6) // Cancella le prime 6 lettere
println(newText)
```

L'output sarà:

```
è una stringa di testo
```

Un'altra opzione è utilizzare il metodo `dropWhile` della classe `Iterable` per eliminare tutti i caratteri iniziali che corrispondono a un modello:

```kotlin
val text = "12345abCDeFGh890"
val newText = text.dropWhile { it.isDigit() } // Elimina tutti i caratteri numerici all'inizio
println(newText)
```

L'output sarà:

```
abCDeFGh890
```

## Vedi anche

- [Documentazione di Kotlin su StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- [ESPLORA.Lingua: Regex in Kotlin](https://explorastudy.wordpress.com/2020/05/29/regex-in-kotlin/)