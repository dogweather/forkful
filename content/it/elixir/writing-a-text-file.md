---
title:                "Scrivere un file di testo"
date:                  2024-01-19
simple_title:         "Scrivere un file di testo"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo è il processo di salvare dati in un formato leggibile. I programmatori lo fanno per persistere informazioni, condividere dati tra processi o conservare log di sistema.

## How to:
Elixir rende la scrittura di file semplice:

```elixir
# Scrivere "Ciao, mondo!" in un file
File.write("saluti.txt", "Ciao, Mondo!")

# Controllare il contenuto del file
IO.puts(File.read!("saluti.txt"))
```
Output:
```
Ciao, Mondo!
```

Per scrivere più linee, usa una lista:

```elixir
# Scrivere più linee in un file
contenuto = ["Ciao, Mondo!", "Benvenuto in Elixir!"]
File.write("saluti.txt", contenuto |> Enum.join("\n"))

# Leggere e stampare il file
IO.puts(File.read!("saluti.txt"))
```
Output:
```
Ciao, Mondo!
Benvenuto in Elixir!
```

## Deep Dive
La manipolazione di file e' stata parte dei linguaggi di programmazione per decenni. In Elixir, la scrittura dei file è gestita dal modulo `File`, che si appoggia al BEAM (Erlang VM) per funzionalità IO efficienti e affidabili. Alternative come `:file.write_file/2` di Erlang sono disponibili, ma `File.write/2` è raccomandata per la maggior parte degli usi. La funzione accetta diversi opzioni, come `:append` per aggiungere al contenuto esistente senza sovrascrivere.

## See Also
- [Elixir `File` Module Documentation](https://hexdocs.pm/elixir/File.html)
- [Programming Elixir (Book by Dave Thomas)](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
- [Elixir School](https://elixirschool.com/en/)
