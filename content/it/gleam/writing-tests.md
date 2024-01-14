---
title:    "Gleam: Scrivere test"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Perché scrivere test con Gleam?

Scrivere test è un'importante parte del processo di sviluppo di software. Con Gleam, è possibile testare il proprio codice in modo semplice ed efficace.

# Come scrivere test con Gleam

Per scrivere test con Gleam, è necessario utilizzare la libreria di test integrata nel linguaggio. Di seguito è riportato un esempio di test di una funzione che trova il massimo numero in una lista:

```Gleam
import gleam/test/assert

fn find_max(numbers) {
  List.fold(numbers, 0, fn(n, max) ->
    if n > max {
      n
    } else {
      max
    }
  )
}

test "find_max with empty list" {
  assert.equal(find_max([]), 0)
}

test "find_max with list of numbers" {
  assert.equal(find_max([1, 5, 3, 2, 4]), 5)
}
```

L'esempio utilizza la funzione `assert.equal` per confrontare il valore ottenuto dalla funzione `find_max` con il valore atteso.

# Approfondimento sui test

Scrivere test ha diversi vantaggi, tra cui la possibilità di individuare eventuali bug nel codice e di garantire il corretto funzionamento delle funzioni. Inoltre, i test possono essere eseguiti automaticamente durante il processo di sviluppo, fornendo un feedback immediato sullo stato del codice.

Gleam fornisce molti altri strumenti utili per scrivere test, come la possibilità di creare mock per testare funzioni complesse e la capacità di eseguire test in parallelo per una maggiore efficienza.

# Vedi anche

- Documentazione ufficiale di Gleam sui test: https://gleam.run/book/testing
- Video tutorial su Gleam e i test: https://www.youtube.com/watch?v=VpnJbjOVk4c