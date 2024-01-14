---
title:                "Gleam: Verifica dell'esistenza di una directory"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Stai lavorando su un progetto di programmazione e sei bloccato perché il tuo codice dipende dall'esistenza di una determinata directory? Niente panico! In questo post ti spiegheremo come verificare se una directory esiste utilizzando il linguaggio di programmazione Gleam.

## Come Fare
Per verificare se una directory esiste nel tuo progetto Gleam, puoi utilizzare la funzione `Filesystem.does_directory_exist/1`. Questo metodo accetta una stringa contenente il percorso della directory da verificare e restituisce `true` o `false` a seconda dell'esistenza o meno della directory.
Ecco un semplice esempio di codice:

```Gleam
let directory = "/percorso/della/directory"

if Filesystem.does_directory_exist(directory) {
  IO.print("La directory esiste!")
} else {
  IO.print("La directory non esiste.")
}
```

Output se la directory esiste: `La directory esiste!`
Output se la directory non esiste: `La directory non esiste.`

## Approfondimento
Ora che hai visto come utilizzare la funzione `Filesystem.does_directory_exist/1` per verificare l'esistenza di una directory, ecco alcuni dettagli aggiuntivi da tenere a mente:
- La funzione può essere utilizzata anche per verificare l'esistenza di un file, poiché un file è essenzialmente una directory vuota.
- Se il percorso specificato nella stringa è un percorso assoluto, la funzione cercherà la directory o il file direttamente nel file system. Se invece è un percorso relativo, verrà cercato nella directory corrente.
- Questa funzione supporta anche percorsi di directory multipli, ad esempio `"/percorso1/percorso2/percorso3"`.

## Vedi Anche
- [Documentazione ufficiale di Filesystem](https://gleam.run/documentation/standard-library/#filesystem)
- [Esempi di codice Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)
- [Community di Gleam](https://github.com/gleam-lang/gleam/blob/master/COMMUNITY.md)