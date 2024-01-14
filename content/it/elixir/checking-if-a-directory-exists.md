---
title:    "Elixir: Verifica se una directory esiste."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché
Controllare se una directory esiste è un'operazione molto comune quando si lavora con il linguaggio di programmazione Elixir. Questo può essere utile per verificare se un determinato file è presente o per gestire correttamente l'input dell'utente nel caso in cui si stia lavorando con file e directory.

## Come Fare
Per verificare se una directory esiste in Elixir, possiamo utilizzare la funzione `File.exists?/1`. Questa funzione accetta come argomento il percorso della directory che si vuole controllare e restituisce un valore booleano, `true` se la directory esiste, `false` se non esiste.

```Elixir
File.exists?("/percorso/della/directory")
# => true
 
File.exists?("non_esistente")
# => false
```

Possiamo anche utilizzare la funzione `File.dir?/1` per verificare se un dato percorso rappresenta una directory o un file. Questa funzione restituisce `:ok` se il percorso corrisponde a una directory, altrimenti restituisce `:error`.

```Elixir
File.dir?("/percorso/della/directory")
# => :ok
 
File.dir?("non_esistente")
# => :error
```

## Approfondimento
Se vogliamo essere più specifici nel nostro controllo, possiamo utilizzare la funzione `File.stat!/1`, che restituisce un elenco di informazioni sul file o sulla directory. Tra queste informazioni, possiamo controllare il campo `:type`, che ci dice se si tratta di una directory o di un file.

```Elixir
File.stat!("/percorso/della/directory").type
# => :directory
 
File.stat!("non_esistente")
# => ** (File.Error) could not read file metadata: no such file or directory
```

## Vedi Anche
- [Documentazione ufficiale di Elixir sulle operazioni sui file](https://elixir-lang.org/getting-started/file-operations.html)
- [Tutorial di Elixir sulla gestione di file e directory](https://www.tutorialspoint.com/elixir/elixir_file_operations.htm)
- [Elixir Forum: Discussione sulla verifica dell'esistenza di una directory](https://elixirforum.com/t/check-if-file-folder-exists-in-elixir/855/2)