---
title:    "Gleam: Leggere un file di testo"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Sei nuovo alla programmazione in Gleam e ti stai chiedendo perché dovresti leggere un file di testo? La risposta è semplice: i file di testo sono uno dei modi più comuni per archiviare e gestire dati. Quindi, se stai lavorando su un progetto che richiede l'accesso ai dati da un file di testo, leggere questo post sarà utile per te.

## Come fare

Per leggere un file di testo in Gleam, puoi utilizzare la funzione `gleam/file.read` passando il percorso del file come argomento. Ad esempio, se vuoi leggere il contenuto di un file di testo chiamato `dati.txt` che si trova nella cartella `documenti`, puoi farlo nel seguente modo:

```Gleam
let file_path = "./documenti/dati.txt"
let file_content = gleam/file.read(file_path)
```

Questo creerà una variabile `file_content` che conterrà il contenuto del tuo file. Puoi anche specificare l'encoding del file tramite il secondo argomento della funzione, se necessario.

Se vuoi leggere solo una parte specifica del file, puoi utilizzare la funzione `gleam/file.read_range`, che accetta un intervallo di caratteri come argomento. Ad esempio, se vuoi leggere solo i primi 10 caratteri del file, puoi farlo nel seguente modo:

```Gleam
let file_path = "./documenti/dati.txt"
let first_10_chars = gleam/file.read_range(file_path, 0, 10)
```

## Approfondimento

Ora che sai come leggere un file di testo in Gleam, è utile conoscere alcune altre funzionalità utili. Ad esempio, puoi usare la funzione `gleam/file.exists` per verificare se un file esiste nel percorso specificato e la funzione `gleam/file.write` per scrivere in un file di testo.

Un'altra funzionalità interessante è la possibilità di usare `gleam/file.lines` per ottenere una lista delle righe in un file di testo. In questo modo puoi scomporre il contenuto in diverse righe e lavorare su di esse separatamente.

## Vedi anche

Ecco alcuni link utili per approfondire la lettura di file di testo in Gleam:

- Documentazione ufficiale di Gleam: https://gleam.run/
- Tutorial sui file di testo in Gleam: https://gleam.run/tutorials/text_files.html
- Altro esempio pratico di lettura di file: https://github.com/gleam-lang/examples/blob/master/file_operations/file.gleam

In bocca al lupo con la tua avventura nella programmazione in Gleam e nella lettura di file di testo!