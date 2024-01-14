---
title:                "PHP: Verifica dell'esistenza di una cartella"
simple_title:         "Verifica dell'esistenza di una cartella"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive un codice PHP, ci sono molte situazioni in cui si tiene traccia di file o directory. Può essere utile verificare se una directory esiste prima di eseguire azioni come creare nuovi file o copiare file in essa. In questo articolo, vedremo come verificare se una directory esiste utilizzando PHP.

## Come fare

Per verificare se una directory esiste utilizzando PHP, possiamo utilizzare la funzione nativa `is_dir()`. Questa funzione restituisce un valore booleano che indica se la directory specificata esiste o meno.

```PHP
<?php
$directory = '/path/to/directory'; // immettere il percorso della directory da verificare
if (is_dir($directory)) { // verifica se la directory esiste
  echo "$directory esiste.";
} else {
  echo "$directory non esiste.";
}
```

Output:
```
/path/to/directory esiste.
```

Se si desidera verificare se una directory esiste all'interno di un'altra directory specifica, è possibile utilizzare `is_dir()` in combinazione con `file_exists()`. Quest'ultima funzione verifica se un file o una directory esiste e restituisce un valore booleano.

```PHP
<?php
$base_directory = '/path/to/base/directory'; // immettere il percorso della directory base
$target_directory = 'directory'; // immettere il nome della directory da verificare
if (file_exists($base_directory . '/' . $target_directory) && is_dir($base_directory . '/' . $target_directory)) {
  echo "La directory $target_directory esiste all'interno di $base_directory.";
} else {
  echo "La directory $target_directory non esiste all'interno di $base_directory.";
}
```

Output:
```
La directory directory non esiste all'interno di /path/to/base/directory.
```

## Approfondimenti

Oltre alla funzione `is_dir()`, esiste anche la funzione `is_readable()` che può essere utilizzata per verificare se una directory è leggibile o meno, e la funzione `is_writable()` per verificare se una directory è scrivibile o meno.

Inoltre, è importante notare che la funzione `is_dir()` potrebbe rallentare l'esecuzione del codice se viene utilizzata su una directory di grandi dimensioni, poiché dovrà scorrere ogni file all'interno della directory per confermare la sua esistenza. In questi casi, è meglio utilizzare la funzione `file_exists()` invece di `is_dir()`.

## Vedi anche

- [Documentazione PHP: is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [Documentazione PHP: file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [Documentazione PHP: is_readable()](https://www.php.net/manual/en/function.is-readable.php)
- [Documentazione PHP: is_writable()](https://www.php.net/manual/en/function.is-writable.php)