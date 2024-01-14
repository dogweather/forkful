---
title:                "Elixir: Stampare l'output di debug"
simple_title:         "Stampare l'output di debug"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un'attività fondamentale per qualsiasi programmatore, in quanto consente di analizzare il comportamento del codice e individuare eventuali errori o bug. In questo articolo, esploreremo come stampare l'output di debug in Elixir e perché è un'abilità importante da possedere.

## Come Fare

Elixir offre diverse opzioni per stampare l'output di debug. Una delle più comuni è utilizzare la funzione `IO.inspect/2`, che stampa una rappresentazione leggibile di qualsiasi valore passato come argomento. Ad esempio:

```elixir
iex> IO.inspect("Hello world")
"Hello world"
:ok
```

È anche possibile utilizzare un'interpolazione di stringhe per stampare valori specifici all'interno di un messaggio di log. Ad esempio:

```elixir
iex> name = "Maria"
"Maria"
iex> IO.puts("Ciao, #{name}!")
Ciao, Maria!
:ok
```

Un'altra opzione è utilizzare la direttiva `Logger.debug/1`, che registra un messaggio di debug nel logger predefinito. In questo modo, è possibile impostare un livello di log diverso per l'output di debug e controllo in diversi ambienti di esecuzione.

## Approfondimento

È importante notare che l'uso di output di debug può avere un impatto significativo sulle prestazioni del codice, quindi è importante utilizzarlo solo quando necessario e rimuoverlo una volta risolti i problemi. Inoltre, è possibile utilizzare alcune funzioni del modulo `IO` come ad esempio `IO.inspect/3`, `IO.puts/2` e `IO.write/2` per specificare opzioni aggiuntive per l'output di debug, come il colore del testo o il livello di indeterminazione.

## Vedi Anche

- [Documentazione su IO](https://hexdocs.pm/elixir/IO.html)
- [Tutorial su come usare Logger in Elixir](https://www.elixir-lang.org/getting-started/logger.html)
- [Articolo su come migliorare l'output di debug in Elixir](https://blog.dnsimple.com/2016/10/elixir-basename-filename-function/)