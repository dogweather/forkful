---
title:                "Kotlin: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte ci troviamo a dover manipolare delle stringhe nel nostro codice, e una delle operazioni più comuni è la capitalizzazione di una stringa. Questo ci permette di modificare facilmente il formato della stringa senza dover scrivere codice aggiuntivo.

## Come fare

Per capitalizzare una stringa in Kotlin, possiamo utilizzare il metodo `capitalize()` che di default converte la prima lettera della stringa in maiuscolo.

```Kotlin
val stringa = "kotlin programming"
println(stringa.capitalize())

// Output: "Kotlin programming"
```

Inoltre, abbiamo la possibilità di specificare una regione e una codifica opzionale nel caso in cui la stringa contenga caratteri speciali.

```Kotlin
val stringa = "è facile iniziare con Kotlin"
println(stringa.capitalize(locale = Locale.ITALIAN))

// Output: "È facile iniziare con Kotlin"
```

Se invece vogliamo capitalizzare l'intera stringa, invece di solo la prima lettera, possiamo utilizzare il metodo `toUpperCase()`.

```Kotlin
val stringa = "kotlin programming"
println(stringa.toUpperCase())

// Output: "KOTLIN PROGRAMMING"
```

## Approfondimento

Se vogliamo ottenere un controllo più preciso sulla capitalizzazione delle stringhe, possiamo utilizzare la classe `StringBuilder` per manipolare la stringa direttamente.

Per esempio, possiamo utilizzare il metodo `setCharAt()` per impostare manualmente la lettera maiuscola nella posizione desiderata.

```Kotlin
val stringa = "kotlin programming"
val stringBuilder = StringBuilder(stringa)
stringBuilder.setCharAt(0, 'K')
println(stringBuilder)

// Output: "Kotlin programming"
```

Inoltre, possiamo utilizzare il metodo `replace()` per sostituire una porzione di stringa specificando gli indici inizio e fine.

```Kotlin
val stringa = "kotlin programming"
val stringBuilder = StringBuilder(stringa)
stringBuilder.replace(0, 6, "Java")
println(stringBuilder)

// Output: "Java programming"
```

## Vedi anche

- [Documentazione ufficiale di Kotlin su `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Documentazione ufficiale di Kotlin su `toUpperCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
- [Documentazione ufficiale di Kotlin su `StringBuilder`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/index.html)