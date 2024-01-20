---
title:                "Verifica se una directory esiste"
html_title:           "Elixir: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Verificare se una directory esiste è un compito comune per molti programmatori: è una tecnica per controllare se un particolare percorso di directory esiste nel file system o no. Questa operazione è vitale per prevenire errori durante la lettura o la scrittura di file.

## Come Fare:

Puoi determinare se una directory esiste o meno in Elixir utilizzando la funzione `File.dir?/1`. Ecco un esempio di utilizzo:

```Elixir
if File.dir?("directory_name") do
  IO.puts "La directory esiste"
else
  IO.puts "La directory non esiste"
end
```

Se la directory esiste, otterrai "La directory esiste" come output. In caso contrario, otterrai "La directory non esiste".

## Approfondimento

### Contesto Storico

Originariamente, l'operazione di verifica dell'esistenza di una directory si basava sugli errori di apertura del file. Successivamente, è stato introdotto il concetto di directory e la necessità di verificare la loro esistenza. Elixir, basato su Erlang/OTP, si affida al modulo `:file` di Erlang per effettuare queste operazioni sul file system.

### Alternative

Un altro approccio consiste nell'utilizzo di un try-rescue-block per tentare di aprire la directory, e gestire l'errore nel caso in cui non esista:

```Elixir
try do
  File.cd!("directory_name")
  IO.puts "La directory esiste"
rescue
  _ -> IO.puts "La directory non esiste"
end
```

### Dettagli Implementativi

`File.dir?/1` è un wrapper attorno al modulo `:file` di Erlang. Controlla se il percorso specificato esiste ed è una directory. Tenere presente che `File.dir?/1` restituisce `true` solo quando il percorso esiste ed è una directory; altrimenti restituisce `false`.

## Vedi Anche:

- Documentazione ufficiale di Elixir 'File' module: https://hexdocs.pm/elixir/File.html
- Per il contesto storico, Erlang ':file' module: http://erlang.org/doc/man/file.html
- Per alternative, Erlang Exception Handling: https://erlang.org/doc/reference_manual/errors.html#runtime-errors