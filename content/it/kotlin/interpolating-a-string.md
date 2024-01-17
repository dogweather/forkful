---
title:                "Interpolazione di una stringa"
html_title:           "Kotlin: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
L'interpolazione di una stringa in Kotlin è il processo di creazione di una stringa che contiene valori inseriti da altre variabili o costanti. I programmatori usano l'interpolazione di stringhe per costruire stringhe complesse in modo più leggibile e organizzato.

## Come fare:
```Kotlin
val name = "Maria"
val age = 25
val message = "Ciao, mi chiamo $name e ho $age anni."
println(message)

//Output:
Ciao, mi chiamo Maria e ho 25 anni.
```

L'esempio sopra mostra come è semplice utilizzare l'interpolazione di stringhe in Kotlin. Basta inserire il valore o la variabile desiderata all'interno delle parentesi graffe all'interno di una stringa.

## Approfondimento:
L'interpolazione di stringhe è diventata popolare grazie a linguaggi di programmazione come Python e Ruby. In altre lingue, è chiamata "formatted string". Se non si utilizza l'interpolazione di stringhe, l'alternativa è concatenare variabili e stringhe usando l'operatore "+".

## Vedi anche:
- [Documentation on string interpolation in Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [Similar concept in Python](https://realpython.com/python-f-strings/)