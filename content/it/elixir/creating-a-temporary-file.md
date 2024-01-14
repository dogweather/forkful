---
title:                "Elixir: Creazione di un file temporaneo"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile in molte situazioni di programmazione. Ad esempio, potresti voler creare un file temporaneo per memorizzare dei dati temporanei durante l'esecuzione del tuo programma. Oppure, potresti aver bisogno di creare un file temporaneo per scrivere dei log di debug o per gestire file temporanei richiesti da alcune librerie.

## Come Fare

Per creare un file temporaneo in Elixir, puoi usare il modulo `Tempfile` della libreria `File`.

```Elixir
alias File.Tempfile
{:ok, temp_file} = Tempfile.open()
```

Con il codice sopra, stiamo usando la funzione `open/0` di `Tempfile` per creare un nuovo file temporaneo e stiamo salvando il file nella variabile `temp_file`. La funzione `open/0` accetta anche dei parametri opzionali, come il nome del prefisso del file e la directory in cui il file deve essere creato.

Una volta che hai il file temporaneo, puoi scriverci del contenuto utilizzando la funzione `write!/2` di `File`.

```Elixir
File.write!(temp_file.path, "Questo è un file temporaneo!")
```

Infine, quando hai finito di usare il file, puoi chiuderlo utilizzando la funzione `close/1` di `Tempfile`.

```Elixir
Tempfile.close(temp_file)
```

## Approfondimento

Creare un file temporaneo in Elixir equibale anche a creare un processo che gestisce il file in modo esclusivo. Ciò significa che solo il processo che ha creato il file temporaneo può accedervi e scriverci. Inoltre, il file viene automaticamente eliminato quando il processo che lo ha creato viene terminato.

È importante notare che, se il processo che crea il file viene terminato in modo anomalo (ad esempio, a causa di un errore), il file temporaneo non verrà eliminato automaticamente. È quindi buona pratica essere certi di chiudere il file in modo esplicito per evitare perdite di risorse.

## Vedi Anche

- [Documentazione di Tempfile](https://hexdocs.pm/elixir/File.Tempfile.html)
- [Come gestire file temporanei in Elixir](https://medium.com/elixir-musings/managing-temporary-files-in-elixir-8c6a0238e1f3)
- [Libreria di gestione file temporanei](https://github.com/elixir-lang/elixir/tree/master/lib/file/tempfile.ex)