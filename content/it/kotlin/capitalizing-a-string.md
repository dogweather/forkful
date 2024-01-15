---
title:                "Maiuscolare una stringa"
html_title:           "Kotlin: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti voler convertire una stringa in maiuscolo, per esempio per stampare un titolo in grassetto o per confrontare due stringhe tutte in maiuscolo. In questo articolo, vedremo come farlo utilizzando il linguaggio di programmazione Kotlin.

## Come fare

Per convertire una stringa in maiuscolo, possiamo utilizzare il metodo `toUpperCase()` della classe `String` di Kotlin. Vediamo un esempio:

```Kotlin
val stringa = "ciao mondo"
val stringaInMaiuscolo = stringa.toUpperCase()
```

Nell'esempio sopra, abbiamo prima definito una variabile `stringa` con il valore "ciao mondo". Poi, abbiamo utilizzato il metodo `toUpperCase()` per convertire la stringa in maiuscolo e abbiamo salvato il risultato nella variabile `stringaInMaiuscolo`.

Possiamo anche utilizzare il metodo `toUpperCase()` direttamente su una variabile di tipo stringa, senza dover prima definire una nuova variabile. Ad esempio:

```Kotlin
val stringa = "saluti!"
println(stringa.toUpperCase()) // stampa "SALUTI!"
```

Se vogliamo convertire una stringa in minuscolo, possiamo utilizzare il metodo `toLowerCase()` della stessa classe `String`, come mostrato nell'esempio seguente:

```Kotlin
val stringa = "HELLO WORLD"
println(stringa.toLowerCase()) // stampa "hello world"
```

Un altro metodo utile è `capitalize()`, che converte la prima lettera della stringa in maiuscolo e lascia le altre in minuscolo. Ad esempio:

```Kotlin
val stringa = "kotlin è un linguaggio di programmazione"
println(stringa.capitalize()) // stampa "Kotlin è un linguaggio di programmazione"
```

## Approfondimento

I metodi `toUpperCase()` e `toLowerCase()` utilizzano l'alfabeto predefinito del sistema operativo per determinare la conversione in maiuscolo o minuscolo dei caratteri. Questo può essere un problema se stiamo lavorando con stringhe in una lingua diversa dall'inglese. 

Per evitare questo problema, possiamo utilizzare il metodo `toUpperCase(Locale)` e `toLowerCase(Locale)` specificando la lingua corretta come parametro. Ad esempio:

```Kotlin
val stringa = "ciao mondo"
val stringaInMaiuscolo = stringa.toUpperCase(Locale.ITALIAN) 
println(stringaInMaiuscolo) // stampa "CIAO MONDO"
```

Come puoi vedere, utilizzando `Locale.ITALIAN` come parametro, il risultato è diverso rispetto all'esempio precedente, dove veniva utilizzato l'alfabeto inglese predefinito.

## Vedi anche

- Documentazione ufficiale dei metodi `toUpperCase()`, `toLowerCase()` e `capitalize()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html
- Altri metodi utili per lavorare con le stringhe in Kotlin: https://kotlinlang.org/docs/reference/strings-overview.html