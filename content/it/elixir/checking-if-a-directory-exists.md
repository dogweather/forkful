---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-19
html_title:           "Arduino: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Controllare l'esistenza di una directory è semplicemente verificare se un certo percorso nel filesystem corrisponde a un posto dove puoi mettere dei file. Lo facciamo per evitare errori durante la lettura o scrittura di file, o prima di creare una nuova directory.

## How to:
Elixir usa il modulo `File` per interagire con il filesystem. Ecco un esempio su come controllare l'esistenza di una directory:

```elixir
if File.dir?("percorso/alla/directory") do
  IO.puts "La directory esiste!"
else
  IO.puts "La directory non esiste."
end
```

Output potrebbe essere:
```
La directory esiste!
```
o
```
La directory non esiste.
```

## Deep Dive
In UNIX, il controllo dell'esistenza di una directory è storicamente fatto usando la syscall `stat` per ottenere metadati di un file o directory. In Elixir, il `File.dir?/1` fa proprio questo sotto il cofano, sfruttando l'interoperabilità con l'ambiente di esecuzione Erlang.

Alternativamente, si può usare `File.ls/1` per elencare i file in una directory e gestire l'`{:error, :enoent}` se la directory non esiste.

```elixir
case File.ls("percorso/alla/directory") do
  {:ok, _files} -> IO.puts "La directory esiste!"
  {:error, :enoent} -> IO.puts "La directory non esiste."
end
```

Dettagli d'implementazione: `File.dir?/1` usa `:filelib.is_dir/1`, che a sua volta utilizza `file:read_file_info/1` della libreria Erlang, per determinare se il percorso specificato è una directory.

## See Also
- Documentazione ufficiale Elixir `File` module: https://hexdocs.pm/elixir/File.html
- Dettagli su syscalls UNIX e `stat`: https://man7.org/linux/man-pages/man2/stat.2.html
- Erlang `file` module: http://erlang.org/doc/man/file.html
