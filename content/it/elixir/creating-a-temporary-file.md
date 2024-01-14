---
title:    "Elixir: Creazione di un file temporaneo"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Sei nuovo alla programmazione in Elixir e vuoi imparare un nuovo concetto utile? Hai bisogno di creare un file temporaneo nel tuo codice? In questo blog post, esploriamo come creare un file temporaneo utilizzando Elixir e come può essere utile nella tua programmazione.

## Perché

Creare un file temporaneo può essere molto utile quando si sta lavorando con dati che non devono essere archiviati permanentemente. Può essere utilizzato per testare nuove funzionalità o per creare dati di supporto temporanei. Inoltre, può essere un modo semplice per gestire la memoria e non sovraccaricare il sistema con file permanenti.

## Come fare

Per creare un file temporaneo in Elixir, possiamo utilizzare la funzione `File.tmp_path/1`. Questa funzione genera un percorso per il file temporaneo nella cartella specificata nel sistema. Possiamo passare il nome della cartella come argomento alla funzione o utilizzare la cartella predefinita di sistema utilizzando `:temp_dir`. Ecco un esempio di codice:

```Elixir
temp_path = File.tmp_path(:temp_dir)
```

Una volta creato il percorso temporaneo, possiamo utilizzare la funzione `File.open/2` per creare il file e scrivere i dati desiderati al suo interno. Ad esempio:

```Elixir
File.open!(temp_path, [:write], fn(file) ->
  IO.write(file, "Questo è un file temporaneo")
end)
```

Possiamo anche ottenere il percorso completo del file temporaneo utilizzando la funzione `File.cwd/0` per ottenere la directory corrente e concatenarla con il percorso del file temporaneo. Ecco un esempio completo:

```Elixir
temp_path = File.tmp_path(:temp_dir)

File.open!(temp_path, [:write], fn(file) ->
  IO.write(file, "Questo è un file temporaneo")
end)

full_path = File.cwd <> temp_path
```

## Approfondimento

Creare un file temporaneo non è l'unico modo per lavorare con dati temporanei in Elixir. Possiamo anche utilizzare la libreria EEx per generare file temporanei utilizzando un modello specificato. Possiamo anche utilizzare la libreria Erlang `:os.getenv/2` per creare file temporanei che sono unici ogni volta che viene eseguito il codice.

## Vedi anche

- Documentazione di Elixir `File` - https://hexdocs.pm/elixir/File.html
- Documentazione di EEx - https://hexdocs.pm/eex/EEx.html
- Documentazione di Erlang `:os` - http://erlang.org/doc/man/os.html