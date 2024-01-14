---
title:                "Kotlin: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Perché
Molti programmatori si trovano spesso a gestire stringhe di testo e a dover estrarre parti specifiche da esse. L'uso di substrings può semplificare notevolmente questa operazione.

# Come fare
Per estrarre una substring da una stringa in Kotlin, è possibile utilizzare il metodo `substring()` fornito dalla classe `String`. Il metodo accetta due parametri: l'indice di inizio e l'indice di fine della substring desiderata.

```Kotlin
val stringa = "Ciao a tutti"
val substring = stringa.substring(5, 7)
println(substring) // Output: a t
```

In questo esempio, la substring sarà compresa tra il carattere alla posizione 5 (incluso) e il carattere alla posizione 7 (escluso) della stringa originale.

Inoltre, è possibile utilizzare i metodi `indexOf()` e `lastIndexOf()` per trovare l'indice di un determinato carattere all'interno di una stringa. Questo può essere utile per determinare gli indici da passare al metodo `substring()`.

# Approfondimento
La classe `String` di Kotlin offre anche altri metodi utili per la gestione delle substrings, come ad esempio `substringAfter()` e `substringBefore()` che permettono di estrarre una substring a partire da una determinata stringa di riferimento.

Inoltre, è possibile utilizzare le espressioni regolari per estrarre substrings in base a determinati pattern. Per farlo, è sufficiente utilizzare il metodo `Regex.find()` che restituirà il match trovato, oppure il metodo `Regex.replace()` per sostituire i match trovati con una nuova stringa.

# Vedi anche
- Documentazione ufficiale di Kotlin sulla classe `String`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string
- Tutorial su espressioni regolari in Kotlin: https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm