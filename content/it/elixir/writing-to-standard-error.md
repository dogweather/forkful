---
title:    "Elixir: Scrivere su errore standard"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché scrivere su standard error?

Scrivere su standard error è un'attività fondamentale nel processo di sviluppo di un'applicazione in Elixir. Questa pratica permette di identificare e risolvere eventuali errori e problemi nel codice in modo rapido ed efficiente.

## Come fare

Per scrivere su standard error in Elixir, è possibile utilizzare la funzione `IO.puts/2` passando come primo argomento la stringa da stampare e come secondo argomento `stderr`. Ad esempio:

```elixir
IO.puts("Errore!", :stderr)
```

Questo stampa la stringa "Errore!" su standard error. È anche possibile utilizzare la macro `IO.warn/1` per stampare un messaggio di avviso su standard error.

## Approfondimento

Scrivere su standard error è particolarmente importante quando si tratta di errori non gestiti nel codice. Questo ci permette di identificare facilmente la causa dell'errore e di risolverlo in modo tempestivo. Inoltre, scrivere su standard error è utile anche per monitorare l'applicazione in fase di produzione e individuare eventuali problemi che possono emergere.

## Vedi anche

- Documentazione ufficiale di Elixir: https://elixir-lang.org/getting-started/io-and-the-file-system.html#stderr-and-stdout
- Articolo su come gestire gli errori in Elixir: https://www.erlang-solutions.com/blog/error-handling-in-elixir.html
- Esempi di utilizzo di `IO.puts/2`: https://hexdocs.pm/elixir/IO.html#puts/2