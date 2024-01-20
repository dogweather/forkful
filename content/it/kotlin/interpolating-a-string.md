---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
L'interpolazione delle stringhe è una funzionalità che consente di incorporare espressioni all'interno delle stringhe stesse. Lo facciamo per rendere il codice più leggibile e conciso, evitando operazioni complicata di concatenazione.

## Come Fare:
In Kotlin, usiamo il simbolo `$` per l'interpolazione delle stringhe. Ecco come:

```Kotlin
fun main() {
    val nome = "Giovanni"
    val eta = 22
    println("Il mio nome è $nome e ho $eta anni")
}
```

L'output sarà:

```
Il mio nome è Giovanni e ho 22 anni
```

Anche le espressioni all'interno delle parentesi graffe (`{}`) possono essere interpolate:

```Kotlin
fun main() {
    val frutta = 5
    val verdura = 7
    println("Ho ${frutta + verdura} pezzi di cibo")
}
```

L'output sarà:

```
Ho 12 pezzi di cibo
```

## Approfondimento
L'interpolazione delle stringhe non è un concetto nuovo. È stata usata per la prima volta nei linguaggi di programmazione Shell e Perl. Alternativamente, si potrebbe utilizzare la concatenazione delle stringhe, ma è più verbosa e può condurre a errori di sintassi. In Kotlin, l'interpolazione delle stringhe è implementata in modo efficiente, poiché l'espressione all'interno del `$` viene valutata prima della restante stringa.

## Vedi Anche
1. [Documentazione ufficiale di Kotlin su Stringhe](https://kotlinlang.org/docs/reference/basic-types.html#string-template)
2. [Tutorial su YouTube dello sviluppatore "Programming with Mosh"](https://www.youtube.com/watch?v=Grf8cJqB4A4)