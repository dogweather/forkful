---
title:    "Gleam: Verifica dell'esistenza di una cartella"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Perché

Controllare l'esistenza di una directory è un'operazione fondamentale quando si lavora con file e cartelle all'interno di un programma Gleam. Questa operazione può essere utile per verificare se una determinata directory esiste prima di crearla o accedervi.

## Come fare

Per controllare se una directory esiste in Gleam, è possibile utilizzare la funzione `FileSystem.dir_exists` passando come argomento il percorso della directory da verificare.

```Gleam
let dir_path = "path/to/directory"
let dir_exists = FileSystem.dir_exists(dir_path)
```

La funzione restituirà un valore booleano, `true` se la directory esiste e `false` se non esiste. Possiamo quindi utilizzare un'istruzione `if` per gestire i diversi scenari:

```Gleam
if dir_exists {
  // La directory esiste
} else {
  // La directory non esiste
}
```

## Approfondimento

È importante notare che la funzione `FileSystem.dir_exists` non controlla solo se la directory esiste, ma anche se è accessibile e se è effettivamente una directory. Se la directory non è accessibile o è stato fornito un percorso che non è una directory, la funzione restituirà `false`.

Inoltre, è possibile utilizzare la funzione `FileSystem.file_exists` per controllare l'esistenza di un file anziché di una directory.

# Vedi anche

- Documentazione ufficiale di `FileSystem`: https://gleam.run/documentation/stdlib/filesystem/
- Creare e accedere a cartelle in Gleam: https://terminated.io/blog/create-access-directories-gleam/