---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
La lettura degli argomenti della riga di comando è un modo per passare le informazioni al programma al suo avvio. Questo permette agli sviluppatori di manipolare il comportamento del programma dinamicamente, rendendo le applicazioni più flessibili ed efficaci.

## Come fare:
Elixir rende semplice la lettura degli argomenti della riga di comando con il modulo `System.argv/0`. Ecco un esempio:

```elixir
defmodule Demo do
  def main do
    IO.inspect(System.argv())
  end
end

Demo.main()
```
Se esegui questo script Elixir con `elixir script.exs arg1 arg2`, vedrai una lista degli argomenti della riga di comando come output: `["arg1", "arg2"]`.

## Approfondimento
La possibilità di leggere gli argomenti della riga di comando è una caratteristica che risale ai primi giorni del programming. Elixir, essendo un linguaggio moderno, rende il processo molto intuitivo e semplice.

Sebbene `System.argv/0` sia il modo più comune di leggere gli argomenti, Elixir offre anche la funzione `OptionParser.parse/2` per interpretare opzioni più complesse. Questa permette di gestire opzioni formattate come `--option value` o `--boolean-option`, che sono una sintassi comune nei programmi di riga di comando.

```elixir
{opts, word, _} = OptionParser.parse(["--word", "hello"], switches: [word: :string])
IO.inspect(opts[:word]) # Stampa: "hello"
``` 

La lettura degli argomenti della riga di comando è implementata nel BEAM (la macchina virtuale su cui gira Elixir) al livello più basso, permettendone un uso efficace e performante.

## Vedi Anche
Per ulteriori informazioni e per approfondire l’argomento, vi suggerisco i seguenti collegamenti:

- Documentazione ufficiale Elixir: [System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0) 
- Documentazione ufficiale Elixir: [OptionParser](https://hexdocs.pm/elixir/OptionParser.html) 
- Elixir School: [Linea di comando](https://elixirschool.com/it/lessons/basics/command-line/)