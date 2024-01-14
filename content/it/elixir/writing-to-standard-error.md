---
title:    "Elixir: Scrivere sulla standard error"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché scrivere su standard error in Elixir?

Scrivere su standard error in Elixir può essere utile per poter visualizzare gli errori ed i messaggi di debug nel terminale. È una pratica comune nella programmazione e può aiutare a identificare e risolvere i problemi all'interno del codice.

## Come: Esempi di codice in Elixir per scrivere su standard error

```Elixir
IO.puts("Questo è un messaggio da scrivere su standard output")
IO.puts("Questa è una frase di esempio da scrivere su standard error")

# Output:
# Questo è un messaggio da scrivere su standard output
# Questa è una frase di esempio da scrivere su standard error
```

Nell'esempio sopra, il primo `IO.puts` scrive sullo standard output mentre il secondo `IO.puts` scrive sullo standard error. Nota che l'output è visualizzato in diverse righe nel terminale. È importante utilizzare `IO.puts` per scrivere su standard error poiché stampa l'output su una nuova linea.

## Deep Dive: Maggiori informazioni su scrivere su standard error

In Elixir, possiamo utilizzare la funzione `IO.puts/2` per scrivere su standard error o possiamo utilizzare la macro `IO.warn/1` per scrivere un messaggio di avviso. La differenza principale tra le due è che `IO.warn/1` aggiunge anche un prefisso "[warning]" al messaggio prima di scriverlo su standard error.

Inoltre, possiamo anche utilizzare la funzione `IO.inspect/2` per scrivere un messaggio di debug su standard error. Questa funzione accetta un valore e lo stampa insieme al nome della variabile per aiutare a identificare il valore corrente durante la fase di debug.

## Vedi anche

- [Documentazione di IO in Elixir](https://hexdocs.pm/elixir/IO.html)
- [Come utilizzare la funzione IO.puts in Elixir](https://www.dneves.dev/blog/how-to-use-io-puts-in-elixir/)
- [Una guida utile per iniziare con Elixir](https://www.scalingdevops.it/elixer-programming-perche-utilizzarlo/)