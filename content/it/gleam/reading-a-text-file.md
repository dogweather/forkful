---
title:                "Leggere un file di testo"
html_title:           "Gleam: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Ciao a tutti! Se vi state chiedendo perché dovreste leggere un file di testo utilizzando Gleam, ecco la risposta: questa azione è fondamentale per poter manipolare e utilizzare i dati contenuti nel file. Inoltre, grazie alla semplicità e all'efficienza di Gleam, leggere un file di testo diventa ancora più semplice e veloce.

## Come farlo

Per leggere un file di testo utilizzando Gleam, seguite questi passaggi:

1. Importate il modulo `gleam/text` nel vostro codice.
2. Utilizzate la funzione `open` per aprire il file specificando il percorso e il modo in cui il file deve essere letto (ad esempio, `read` o `read_lines`).
3. Utilizzate la funzione `read` per leggere il contenuto del file in una variabile di tipo `Text`.
4. Per manipolare il contenuto del file, potete utilizzare le funzioni disponibili nel modulo `gleam/text`.
5. Ricordate di chiudere il file utilizzando la funzione `close`.

Ecco un esempio di codice che legge il contenuto di un file di testo e stampa ogni riga:

```gleam
import gleam/text

// Apro il file in modalità lettura
let file = text.open("percorso_del_file.txt", read_lines)

// Leggo il contenuto e lo assegno a una variabile
let contenuto = text.read(file)

// Stampo ogni riga del file
for line in contenuto {
  io.println(line)
}

// Chiudo il file
text.close(file)
```

Esempio di output:

```
Questa è la prima riga del file.
Questa è la seconda riga del file.
Questa è la terza riga del file.
```

## Approfondimento

Ora che sapete come leggere un file di testo utilizzando Gleam, ecco alcune informazioni aggiuntive che possono esservi utili:

- Il formato dei file di testo viene specificato nel modulo `gleam/encoding`. Potete utilizzare la funzione `detect` per determinare il formato di un file.
- Se il vostro file contiene dati strutturati, come ad esempio un file CSV o JSON, potete utilizzare i moduli `gleam/csv` e `gleam/json` per analizzarli in modo più efficiente.

## Vedi anche

- Documentazione ufficiale di Gleam su "Leggere un file di testo": https://gleam.run/articles/text-files
- Tutorial su come leggere un file di testo utilizzando Gleam: https://www.youtube.com/watch?v=dQw4w9WgXcQ (scherzo, non esiste ancora un tutorial su questo argomento)
- Risoluzione dei problemi più comuni durante la lettura di file di testo con Gleam: https://gleam.run/articles/text-files-troubleshooting (inglese)