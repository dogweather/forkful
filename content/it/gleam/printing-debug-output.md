---
title:                "Gleam: Stampa di output di debug"
simple_title:         "Stampa di output di debug"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché
Scrivere messaggi di debug durante la programmazione può aiutare a comprendere meglio il funzionamento del codice e gli eventuali errori che possono verificarsi durante l'esecuzione. Inoltre, può essere utile per scopi di testing e per individuare eventuali bug nel software.

## Come fare
In Gleam, è possibile stampare i messaggi di debug utilizzando la funzione `io.debug()`. Ad esempio, per stampare una stringa di testo è sufficiente scrivere:

```Gleam
io.debug("Il codice sta eseguendo correttamente")
```

Questo comando stampa il messaggio all'interno della console di debug. È possibile anche utilizzare la funzione `io.debug()` per stampare il valore di una variabile o di un'espressione, come mostrato nell'esempio seguente:

```Gleam
let number = 42
io.debug(number)
// Output: 42
```

È possibile stampare più valori in una sola volta, separandoli da una virgola all'interno della funzione `io.debug()`. Ad esempio:

```Gleam
let name = "Marco"
let age = 25
io.debug("Il mio nome è", name, "e ho", age, "anni.")
// Output: Il mio nome è Marco e ho 25 anni.
```

## Profondità
Stampare messaggi di debug può essere utile anche per monitorare il flusso del codice e identificare eventuali problemi nelle logiche di programmazione. Inoltre, è possibile utilizzare la funzione `io.debug()` per stampare i valori di un tipo di dato personalizzato definito dall'utente.

Ad esempio, supponiamo di avere il seguente tipo di dato `User` con le seguenti proprietà:

```Gleam
type User {
  name: String,
  age: Int,
  email: String,
}
```

Per stampare i dettagli di un utente, possiamo utilizzare il seguente codice:

```Gleam
let user = User("Marco", 25, "marco@gmail.com")
io.debug(user.name, user.age, user.email)
// Output: Marco, 25, marco@gmail.com
```

Inoltre, la funzione `io.debug()` accetta anche espressioni condizionali, che possono essere utili per stampare un messaggio di debug solo se una determinata condizione è soddisfatta. Ad esempio:

```Gleam
let number = 24
if number > 30 {
  io.debug("Il numero è maggiore di 30")
} else {
  io.debug("Il numero è minore di 30")
}
// Output: Il numero è minore di 30
```

## Vedi anche
- Documentazione ufficiale di Gleam: https://gleam.run/
- Tutorial su come utilizzare la funzione `io.debug()`: https://gleam.run/book/tour/io.html
- Esempi di debug in Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/debug.gleam