---
title:                "Verifica se una directory esiste"
html_title:           "Gleam: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Verificare se una directory esiste è l'atto di controllare se un dato percorso del file system contiene una specifica directory. I programmatori lo fanno per prevenire errori durante le operazioni di lettura/scrittura di file.

## Come si fa:

Ecco un esempio di codice semplice con l'output previsto.

```Gleam
import gleam/filesystem

let esiste = filesystem.directory_exists("nome_directory")
case esiste {
  Some(_) -> io.println("La directory esiste")
  None -> io.println("La directory non esiste")
}
```

L'output sarà "La directory esiste" se la directory esiste e "La directory non esiste" altrimenti.

## Approfondimenti

1. **Contesto storico**: la funzionalità di controllo dell'esistenza di una directory è stata introdotta per la prima volta nei primi sistemi operativi come Unix. Da allora, è divenuta una pratica standard in molti linguaggi di programmazione, compreso Gleam.

2. **Alternative**: in Gleam, potresti anche utilizzare `filesystem.read_directory` e catturare un'eccezione se la directory non esiste. Tuttavia, questo approccio è più lento e meno elegante.

3. **Dettagli di implementazione**: La funzione `filesystem.directory_exists` di Gleam funziona aprendo semplicemente la directory specificata. Se riesce, ritorna `Some(.)`. Se fallisce, ritorna `None`.

## Collegamenti utili

1. [La documentazione ufficiale di Gleam](https://gleam.run/documentation/)
2. [Gleam su GitHub](https://github.com/gleam-lang/gleam)
3. [Funzioni di filesystem in Gleam](https://gleam.run/stdlib/gleam_filesystem/)