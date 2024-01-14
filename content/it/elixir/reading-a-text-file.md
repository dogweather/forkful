---
title:    "Elixir: Leggere un file di testo"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è una delle attività più comuni che un programmatore deve svolgere quando lavora con dati in un'applicazione. Questo articolo esplorerà come leggere un file di testo utilizzando il linguaggio di programmazione Elixir. Continua a leggere per scoprire come.

## Come Fare

Per prima cosa, è necessario utilizzare la funzione `File.read/1` di Elixir per leggere il contenuto di un file di testo. Questa funzione accetta il percorso del file come argomento e restituisce i dati letti sotto forma di stringa. Vediamo un esempio:

```Elixir
contents = File.read("mio_file.txt")
```

Il risultato ottenuto sarà una variabile `contents` contenente il contenuto del file come stringa. Ora puoi utilizzare questa variabile per elaborare i dati come desideri nella tua applicazione.

Se vuoi leggere solo una determinata quantità di caratteri o linee dal file, puoi utilizzare le funzioni `File.stream!/3` o `File.stream!/2`. Queste funzioni restituiscono uno stream che può essere elaborato ulteriormente o convertito in una lista o array. Ecco un esempio:

```Elixir
linee = File.stream!("mio_file.txt") |> Enum.take(5)
```

Questo leggerà solo le prime 5 linee del file e le memorizzerà nella variabile `linee` sotto forma di lista.

## Approfondimento

Per leggere file di testo di grandi dimensioni, è consigliabile utilizzare la funzione `File.stream!/3` in combinazione con la funzione `Enum.reduce/3`. Questo eviterà di caricare l'intero file in memoria e consente di elaborare i dati in modo efficiente.

```Elixir
File.stream!("grande_file.txt") |> Enum.reduce(0, fn line, sum ->
  # esegui operazioni sulle singole linee del file
  sum + line |> String.length
end)
```

In questo esempio, la somma dei caratteri in ogni linea viene calcolata utilizzando `Enum.reduce/3` e viene restituito il risultato finale.

## Vedi Anche

- [Documentazione ufficiale su File](https://hexdocs.pm/elixir/File.html)
- [Tutorial su come leggere un file di testo in Elixir](https://pragmaticstudio.com/tutorials/reading-files-in-elixir)
- [Elixir School - Lettura e scrittura dei file](https://elixirschool.com/it/lessons/basics/io/)

Grazie per aver letto questo articolo. Speriamo che ti sia stato d'aiuto per capire come leggere un file di testo utilizzando Elixir. Buona programmazione!