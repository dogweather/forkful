---
title:                "Gleam: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è uno dei fondamenti più importanti della programmazione. Ci permette di comunicare con il nostro codice e di fornire istruzioni precise al nostro computer.

## Come Fare

Per scrivere un file di testo in Gleam, è necessario prima importare il modulo `File`, che ci permette di interagire con i file sul nostro sistema.

```
Gleam import File
```

Successivamente, possiamo utilizzare la funzione `File.write` per creare un nuovo file di testo vuoto e scrivere al suo interno.

```
Gleam File.write("test.txt") "Questo è un esempio di testo da scrivere nel file!"
```

Una volta eseguito, controllando la nostra cartella di lavoro, dovremmo vedere un nuovo file di testo chiamato "test.txt" con il nostro testo inserito al suo interno.

## Approfondimento

Scrivere un file di testo in Gleam è utile non solo per fornire istruzioni ai nostri programmi, ma anche per creare output visibili all'utente. Possiamo anche utilizzare la funzione `File.write` per scrivere variabili al suo interno, come ad esempio una lista di numeri.

```
Gleam File.write("numeri.txt") [1, 2, 3, 4, 5]
```

Ciò creerà un nuovo file di testo chiamato "numeri.txt" con la lista di numeri al suo interno, utile ad esempio per generare report o output articolati.

## Vedi Anche

- Documentazione ufficiale di Gleam: https://gleam.run/
- Tutorial su come scrivere file di testo in Gleam: https://gleam.run/documentation/tutorials/file-writing/