---
title:                "Utilizzo di un interprete interattivo (REPL)"
date:                  2024-01-26T04:13:00.553263-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Un shell interattivo, o REPL (Read-Eval-Print Loop, Ciclo di Lettura-Valutazione-Stampa), permette di provare frammenti di codice in tempo reale. I programmatori Elixir usano il REPL, chiamato IEx (Interactive Elixir), per sperimentare, effettuare debug e apprendere il linguaggio.

## Come fare:
Per avviare IEx, apri il terminale e digita `iex`. Ecco un assaggio:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

L'output dovrebbe mostrare l'assegnazione di variabili, i risultati delle funzioni e una funzione anonima al lavoro.

## Approfondimenti
La shell IEx è parte di Elixir fin dai suoi primi giorni. José Valim, il creatore di Elixir, ha tratto ispirazione dalle shell interattive di altri linguaggi come il `python` di Python e il `irb` di Ruby. Sebbene IEx condivida molte funzionalità con questi, è costruito per gestire la natura concorrente di Elixir ed è completamente integrato con le capacità della Erlang VM.

Alternative a IEx nell'ecosistema Erlang includono `erl`, la shell di Erlang. Ma IEx fornisce un ambiente più amichevole per Elixir, con funzionalità come completamento automatico avanzato, cronologia e assistenti.

Il REPL IEx è più di un parco giochi; può connettersi in modo trasparente a un sistema in esecuzione. Questo è cruciale per il debug di applicazioni live. L'implementazione sottostante si basa su BEAM (la Erlang VM), assicurando che funzionalità come lo scambio di codice a caldo siano supportate direttamente nella shell.

## Vedi anche
Consulta questi link per ulteriori letture e risorse:

- [Documentazione di IEx di Elixir](https://hexdocs.pm/iex/IEx.html)
- [Interactive Elixir (IEx) - La Shell di Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Documentazione di `erl` di Erlang](http://erlang.org/doc/man/erl.html)
- [Imparare la Shell Interattiva di Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)