---
title:                "Gleam: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

La lettura di file di testo è un'operazione fondamentale nella programmazione di Gleam. Conoscere il processo aiuta a gestire e manipolare i dati correttamente.

## Come fare

Per prima cosa, dobbiamo importare la libreria standard `file` di Gleam:

```
import file
```

Successivamente, possiamo utilizzare la funzione `read` per leggere i contenuti di un file di testo e salvarli in una variabile. Ad esempio, se vogliamo leggere il file `text.txt` presente nella stessa directory del nostro codice, possiamo farlo in questo modo:

```
let file_content = file.read("text.txt")
```

Possiamo anche specificare un percorso assoluto se il file è in una cartella differente, come ad esempio:

```
let file_content = file.read("/home/user/text.txt")
```

Una volta ottenuto il contenuto del file, possiamo stamparlo a schermo utilizzando la funzione `io.println` della libreria standard `io`:

```
io.println(file_content)
```

Questo esempio stampa il contenuto del file in una riga singola, ma possiamo utilizzare il metodo `split` della libreria standard `string` per dividere il testo in più righe, come ad esempio:

```
let file_lines = string.split(file_content, "\n")
```

Questa operazione crea una lista di stringhe, dove ogni elemento corrisponde ad una riga del file. Possiamo quindi utilizzare un ciclo `for` per stampare ogni riga a schermo:

```
for line in file_lines {
  io.println(line)
}
```

## Approfondimento

La funzione `read` accetta una stringa che rappresenta il percorso del file come primo argomento e restituisce una stringa contenente tutto il testo presente nel file. Tuttavia, questa funzione può anche accettare un secondo argomento opzionale di tipo `Options` che permette di specificare dei parametri aggiuntivi per la lettura del file, come ad esempio:

- `encoding`: indica il tipo di codifica del testo nel file (è possibile utilizzare standard come `utf-8` o `ascii`);
- `line_ending`: indica il tipo di separatore di righe (come ad esempio `\n` in sistemi Unix o `\r\n` in sistemi Windows);
- `max_bytes`: indica la dimensione massima del file che si vuole leggere in byte.

Ad esempio, se vogliamo leggere un testo in cinese presente in un file codificato in `gb18030` e con le righe terminate da `\n`:

```
let options = file.Options(max_bytes: 1024, encoding: "gb18030", line_ending: "\n")
let file_content = file.read("cinese.txt", options)
```

## Vedi anche

- Documentazione ufficiale della libreria `file`: https://gleam.run/documentation/standard-library/file/
- Documentazione ufficiale della libreria `io`: https://gleam.run/documentation/standard-library/io/
- Documentazione ufficiale della libreria `string`: https://gleam.run/documentation/standard-library/string/