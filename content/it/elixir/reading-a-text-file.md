---
title:                "Elixir: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano spesso a dover lavorare con file di testo per leggere e manipolare dati. Con Elixir, questo compito può essere eseguito facilmente e in modo efficiente utilizzando le sue funzionalità di lettura file.

## Come Fare

Per leggere un file di testo con Elixir, è necessario utilizzare la funzione `File.read/1` e specificare il percorso del file come argomento. Ad esempio, se abbiamo un file chiamato "dati.txt" nella stessa cartella del nostro codice, possiamo utilizzare la seguente sintassi:

```Elixir
File.read("dati.txt")
```

Se vogliamo leggere tutti i dati del file come stringa, possiamo utilizzare la funzione `File.read!/1` che restituisce i dati come stringa invece di una tupla. Ad esempio:

```Elixir
File.read!("dati.txt")
```

Inoltre, possiamo specificare il formato dei dati che stiamo leggendo aggiungendo un secondo argomento alla funzione `File.read/2`. Ad esempio, se il nostro file contiene dati in formato CSV, possiamo specificare `:csv` come secondo argomento e i dati verranno restituiti come una lista di liste con ogni elemento rappresentante una riga del file.

```Elixir
File.read("dati.csv", :csv)
```

## Approfondimento

Quando leggiamo un file di testo con Elixir, il contenuto viene letto e restituito come una tupla contenente il risultato della lettura e l'errore. Possiamo quindi gestire facilmente eventuali errori utilizzando pattern matching.

Inoltre, è possibile specificare diverse opzioni di lettura per il file utilizzando la funzione `File.open/2`. Ad esempio, possiamo specificare l'encoding del file, la modalità di lettura (lettura o scrittura) e altre opzioni avanzate.

Per ulteriori informazioni sulla lettura di file di testo con Elixir, si consiglia di consultare la documentazione ufficiale.

## Vedi Anche

- [Documentazione su File.read/1](https://hexdocs.pm/elixir/File.html#read/1)
- [Documentazione su File.open/2](https://hexdocs.pm/elixir/File.html#open/2)
- [Tutorial su file di testo in Elixir](https://www.tutorialspoint.com/elixir/elixir_file_io.htm)