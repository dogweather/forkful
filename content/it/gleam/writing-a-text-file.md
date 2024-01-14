---
title:                "Gleam: Scrivere un file di testo"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Scrivere un file di testo è essenziale per la creazione di programmi che possono immagazzinare e leggere dati, come ad esempio configurazioni o risultati di calcoli.

## Come fare
Per scrivere un file di testo in Gleam, è necessario utilizzare la funzione `file.write` e specificare il nome del file e il contenuto da scrivere. Ad esempio:

```Gleam
let result =
  file.write("output.txt", "Questo è il mio primo file di testo in Gleam!")

file.match(result) {
  Ok(_) -> "File scritto con successo!"
  Error(_) -> "Errore nella scrittura del file."
}
```

L'output di questo esempio sarà una stringa con il messaggio "File scritto con successo!" se la scrittura è andata a buon fine, altrimenti una stringa con il messaggio di errore.

## Approfondimenti
Per gestire situazioni più complesse durante la scrittura di un file di testo, è possibile utilizzare altre funzioni come `file.append` per aggiungere contenuti a un file esistente, o `file.delete` per eliminare un file. Inoltre, è importante tenere in considerazione il tipo di codifica del file, poiché Gleam supporta sia la codifica dei caratteri UTF-8 che quella ISO-8859-1.

## Vedi anche
- Documentazione ufficiale di Gleam su `file`
- Tutorial su scrittura di file in Gleam
- Esempi di codice per la scrittura di file in Gleam