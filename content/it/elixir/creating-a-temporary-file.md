---
title:                "Creazione di un file temporaneo"
html_title:           "Elixir: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere codice può essere molto divertente, ma a volte può anche essere un po' frustrante. Ci capita spesso di voler creare un file temporaneo per eseguire dei test o per salvare dei dati temporanei, ma non sappiamo bene come farlo in modo efficiente. Fortunatamente, Elixir ha una soluzione molto semplice e potente per questo problema: i file temporanei.

## Come Fare

Per creare un file temporaneo in Elixir, è necessario utilizzare la funzione `Path.tmpname/2`. Questa funzione prende come parametri la directory in cui si vuole creare il file e un prefisso opzionale per il nome del file. Ad esempio:

```Elixir
path = Path.tmpname("tmp/", "tempfile-")
```

Questo genererà un nome casuale per il file nella directory "tmp/" con il prefisso "tempfile-". Per creare effettivamente il file, è possibile utilizzare la funzione `File.open/2` e passare come secondo parametro l'opzione `:write`. Inoltre, è possibile specificare il tipo di codifica desiderato per il file utilizzando l'opzione `:encoding` come mostrato nell'esempio seguente:

```Elixir
{:ok, file} = File.open(path, [:write, :encoding, :utf8])
```

Una volta che abbiamo creato il file, possiamo scriverci all'interno utilizzando la funzione `IO.write/2` e passando come primo parametro il file appena creato e come secondo parametro i dati da scrivere. Alla fine, è importante chiudere il file utilizzando la funzione `IO.close/1`.

```Elixir
IO.write(file, "Questo è un test!")
IO.close(file)
```

## Approfondimento

Una cosa da tenere a mente è che i file temporanei creati in questo modo verranno automaticamente eliminati quando il processo viene terminato. Questo è molto utile per evitare di dover gestire manualmente l'eliminazione dei file temporanei dopo averli utilizzati. Se si vuole comunque eliminare il file prima della terminazione del processo, è possibile utilizzare la funzione `File.rm/1`.

Un'altra cosa importante da notare è che si possono specificare più opzioni dopo il prefisso del nome del file per ottenere maggiori informazioni sul file creato. Ad esempio:

```Elixir
path = Path.tmpname("tmp/", "tempfile-", [:binary, :statistics])
```

in questo caso, il file creato conterrà solo dati binari e sarà possibile accedere alle sue informazioni utilizzando la funzione `File.stat/1`.

## Vedi Anche

- [La documentazione di Elixir sulle funzioni per la gestione dei file](https://hexdocs.pm/elixir/File.html)
- [Un articolo su come creare file temporanei in Python](https://realpython.com/temporary-files-python/)