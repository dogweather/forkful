---
title:                "Scrivere su errore standard"
html_title:           "Elixir: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere su standard error significa inviare messaggi di errore al terminale mentre si esegue un programma. I programmatori lo fanno per informare gli utenti del programma di eventuali problemi durante l'esecuzione e per aiutare a risolverli.

## Come:

### Esempio 1:
```Elixir
IO.puts("Messaggio di output") # stampa su standard output
IO.puts(:stderr, "Messaggio di errore") # stampa su standard error
```

Output:
```
Messaggio di output
Messaggio di errore
```

### Esempio 2:
```Elixir
File.read("file_inesistente") # tenta di leggere un file inesistente
|> case do
  {:ok, result} -> IO.puts(result) # stampa il contenuto del file se viene letto con successo
  {:error, reason} -> IO.puts(:stderr, "Errore di lettura: #{reason}") # invia un messaggio di errore su standard error
end
```

Output:
```
Errore di lettura: no such file or directory
```

## Approfondimento:

### Contesto storico:
Scrivere su standard error è una pratica comune nei linguaggi di programmazione e nei sistemi operativi da diversi decenni. L'idea è nata come soluzione per differenziare i messaggi di debugging da quelli di output regolari.

### Alternative:
Esistono diverse alternative per gestire gli errori durante l'esecuzione di un programma, come l'uso di eccezioni o la stampa dei messaggi di errore direttamente nel codice dell'applicazione. Tuttavia, scrivere su standard error rimane una pratica efficace e consolidata per gestire gli errori.

### Dettagli di implementazione:
In Elixir, è possibile utilizzare la funzione `IO.puts/2` per scrivere su standard error invece che su standard output. È inoltre possibile utilizzare la macro `Macro.stderr/1` per ottenere il risultato equivalente.

## Vedi anche:

- [Documentazione di IO.puts/2 in Elixir](https://hexdocs.pm/elixir/IO.html#puts/2)