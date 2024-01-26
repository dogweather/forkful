---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:35:29.229748-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenare le stringhe significa unire due o più testi in uno solo. Lo facciamo per creare messaggi dinamici o per lavorare con dati input-output.

## How to:
Concatenare con l'operatore `+`:
```Kotlin
val saluto = "Ciao"
val nome = "Marco"
val messaggio = saluto + ", " + nome + "!"
println(messaggio) // Output: Ciao, Marco!
```

Usare `concat`:
```Kotlin
val stringa1 = "Kotlin "
val stringa2 = "è fico."
val risultato = stringa1.concat(stringa2)
println(risultato) // Output: Kotlin è fico.
```

Interpolazione di stringhe con `$`:
```Kotlin
val animale = "gatto"
val eta = 3
val frase = "Il mio $animale ha $eta anni."
println(frase) // Output: Il mio gatto ha 3 anni.
```

Usare `StringBuilder`:
```Kotlin
val builder = StringBuilder()
builder.append("Kotlin")
builder.append(" è")
builder.append(" versatile!")
println(builder.toString()) // Output: Kotlin è versatile!
```

## Deep Dive
Concatenare stringhe è basilare nella programmazione, usato fin dagli albori del software. In Kotlin, l'operatore `+` è semplice ma può essere subottimale per la memoria se usato in loop intensivi a causa dell'allocazione di nuovi oggetti stringa. L'interpolazione di stringhe è più leggibile e performante, specialmente con vars e expressions all'interno delle stringhe. `StringBuilder` è utile quando c'è da costruire una stringa in molti passaggi o in cicli, riducendo il sovraccarico di memoria.

In Java, concatenare con operatore `+` si traduce in una conversione implicita a `StringBuilder`, ma in Kotlin questo avviene solo dentro a loop, non per semplici espressioni concatenate. Alternative come `joinToString` o `format`, meno usate, permettono un controllo più fine su formattazione e localizzazione.

## See Also
- Kotlin `StringBuilder` documentation: [StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- Discussion on string performance: ["Kotlin String Interpolation"](https://stackoverflow.com/questions/46520907/why-is-kotlin-string-interpolation-implemented-as-template-rather-than-simple-st) on Stack Overflow
