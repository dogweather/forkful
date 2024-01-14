---
title:                "PHP: Verificare se una directory esiste"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Esistono molte situazioni in cui può essere utile verificare se una directory esiste o meno. Ad esempio, quando si vuole creare una nuova directory per archiviare i file o quando si vuole eliminare una directory solo se esiste già.

## Come Fare

Per verificare se una directory esiste, possiamo utilizzare la funzione `is_dir()` di PHP. Questa funzione accetta come argomento il percorso della directory e restituisce un valore booleano, `true` se la directory esiste e `false` se non esiste.

Ecco un esempio di codice in cui utilizziamo la funzione `is_dir()` per verificare l'esistenza di una directory chiamata "documenti":

```PHP
if(is_dir("documenti")){
  echo "La directory 'documenti' esiste.";
} else{
  echo "La directory 'documenti' non esiste.";
}
```

Se la directory "documenti" esiste sulla nostra macchina, il codice stamperà "La directory 'documenti' esiste." Altrimenti, stamperà "La directory 'documenti' non esiste."

## Approfondimento

Oltre alla funzione `is_dir()`, PHP offre anche altre funzioni per gestire le directory. Alcune di queste sono:

- `mkdir()`: per creare una nuova directory
- `rmdir()`: per eliminare una directory vuota
- `scandir()`: per ottenere un array di tutti i file e le directory presenti all'interno di una directory specificata

È importante notare che quando si utilizzano queste funzioni, il percorso della directory deve essere specificato correttamente per ottenere i risultati desiderati.

## Vedi Anche

- [Documentazione su `is_dir()` di PHP](https://www.php.net/manual/en/function.is-dir.php)
- [Tutorial su come gestire le directory in PHP](https://www.tutorialrepublic.com/php-tutorial/php-file-directory-management.php)
- [Video introduttivo su is_dir() e altre funzioni di gestione delle directory](https://www.youtube.com/watch?v=IJkTWHa4y2E)