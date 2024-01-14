---
title:                "Elixir: Leggere un file di testo"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è un'operazione comune durante lo sviluppo di software. Può essere utile per leggere dati da un file di configurazione o per processare un grande set di dati. In questo articolo, scopriremo come leggere un file di testo utilizzando Elixir.

## Come

Per prima cosa, dobbiamo aprire il file di testo utilizzando la funzione `File.open/2`. Questa funzione accetta due argomenti: il percorso del file e una lista di opzioni. Per leggere un file di testo, possiamo passare l'opzione `:read` alla funzione.

Dopo aver aperto il file, possiamo utilizzare la funzione `IO.read/2` per leggere il contenuto del file. Questa funzione accetta il file aperto come primo argomento e il numero di byte da leggere come secondo argomento. Se non specificato, il numero di byte predefinito è 8192.

Una volta letto il contenuto del file, è importante chiudere il file utilizzando la funzione `File.close/1` per evitare perdite di memoria.

Di seguito è riportato un esempio di codice che legge il contenuto di un file di testo e lo stampa nella console utilizzando la funzione `IO.puts/1`:

```Elixir
file = File.open("testo.txt", [:read])
content = IO.read(file)
IO.puts(content)
File.close(file)
```

L'output sarà il contenuto del file di testo.

## Deep Dive

Quando si lavora con file di grandi dimensioni, è meglio leggere il contenuto in blocchi anziché leggerlo in una sola volta. Possiamo utilizzare la funzione `IO.binread/2` per fare ciò. Questa funzione accetta il file aperto come primo argomento e il numero di byte da leggere come secondo argomento. Restituirà il contenuto del file come binario, che può essere facilmente convertito in stringa utilizzando la funzione `IO.iodata_to_binary/1`.

Un'altra funziona utile per leggere file di grandi dimensioni è `Stream.resource/3`. Questa funzione accetta tre argomenti: uno stato iniziale, una funzione da chiamare per generare ogni elemento dello stream e una funzione da chiamare per terminare lo stream. Possiamo utilizzare questa funzione per leggere il contenuto di un file in modo efficiente e senza occupare troppa memoria.

## See Also (Vedi anche)

- [File - Elixir Documentation](https://hexdocs.pm/elixir/File.html)
- [IO - Elixir Documentation](https://hexdocs.pm/elixir/IO.html)
- [Stream - Elixir Documentation](https://hexdocs.pm/elixir/Stream.html)