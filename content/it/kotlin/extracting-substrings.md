---
title:                "Estrazione di sottostringhe"
html_title:           "Kotlin: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

Che cos'è e perché lo facciamo?
L'estrazione di sottostringhe è una tecnica utilizzata dai programmatori per ottenere una parte specifica di una stringa più lunga. È utile quando si lavora con dati in formato stringa, come nel caso di analisi del testo o manipolazione di URL. 

Come farlo:
```kotlin
val stringa = "Ciao, sono una stringa di esempio"
val sottostringa = stringa.substring(6, 13)

println(sottostringa) // output: "sono una"
```

Se il metodo di estrazione richiede che la prima sottostringa sia inclusa e l'ultima esclusa. Quindi, nel nostro esempio, il carattere iniziale è il sesto e l'ultimo è il dodicesimo. Il codice ci restituirà la stringa da "s" a "e", inclusi. 

Approfondimento:
Questo processo è conosciuto anche come "slicing" e ha le sue origini nella programmazione di linguaggi come Python. Un'alternativa al metodo di estrazione di Kotlin è l'utilizzo dell'operatore `[]` come in JavaScript. Implementato in modo efficiente grazie alla gestione di stringhe come array di caratteri, il metodo di estrazione applica una coppia di indici all'oggetto di tipo String e restituisce la sottostringa corrispondente. 

Vedi anche:
- [Documentazione di Kotlin sul metodo substring()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Spiegazione dettagliata dei metodi di estrazione di Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)