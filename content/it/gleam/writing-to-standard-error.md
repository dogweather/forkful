---
title:                "Gleam: Scrivere su standard error"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile per visualizzare errori o avvisi importanti durante l'esecuzione di un programma, così come per il debug e il testing del codice.

## Come

Per scrivere su standard error in Gleam, possiamo utilizzare la funzione `io.write_stderr` fornita dalla libreria standard. Vediamo un esempio di codice:

```gleam
import gleam/io

// Scrive una stringa su standard error
io.write_stderr("Questo è un messaggio di errore")

// Possiamo anche formattare la stringa utilizzando il placeholder %s
let name = "Carlo"
io.write_stderr("Ciao %s, benvenuto su standard error!", [name])
```

L'output di questo codice sarà visualizzato su standard error:

```
Errore: Questo è un messaggio di errore
Ciao Carlo, benvenuto su standard error!
```

Possiamo anche scrivere tipi personalizzati su standard error utilizzando `io.format`, come mostrato nell'esempio seguente:

```gleam
import gleam/io
import gleam/string

pub struct Person(name: String, age: Int)

// Scrive una istanza della struct Person su standard error
let person = Person("Luca", 30)
io.format_stderr_person("Benvenuto %s, hai %i anni", [person.name, person.age])
```

L'output di questo codice sarà:

```
Benvenuto Luca, hai 30 anni
```

## Deep Dive

In Gleam, standard error è rappresentato dal tipo `io::StandardError`, che ci consente di scrivere su di esso utilizzando le funzioni `write` e `format`. Possiamo anche utilizzare i moduli `io::format` e `io::format_builder` per una formattazione più avanzata delle nostre stringhe. Inoltre, possiamo anche catturare gli errori scritti su standard error utilizzando il modulo `gleam/io/test`.

## Vedi Anche

- Documentazione ufficiale di Gleam su `io`
- Tutorial su standard error in Gleam