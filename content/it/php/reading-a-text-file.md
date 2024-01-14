---
title:    "PHP: Lettura di un file di testo"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché leggere un file di testo

Leggere un file di testo può essere utile per estrarre informazioni o dati importanti da un documento. Inoltre, può essere utilizzato per analizzare grandi quantità di testo o per manipolare dati all'interno di un file.

## Come leggere un file di testo in PHP

```PHP
$file = fopen("testo.txt", "r"); // apre il file in modalità di sola lettura
if ($file) {
  while (($line = fgets($file)) !== false) { // legge ogni riga del file
    echo $line . "<br>"; // stampa il contenuto della riga
  }
  fclose($file); // chiude il file
} else {
  echo "Impossibile aprire il file.";
}
```

Output:

```
Prima riga del file. 
Seconda riga del file.
Terza riga del file.
```

## Approfondimento sulla lettura di un file di testo

Per leggere un file di testo, è fondamentale utilizzare la funzione `fopen()` che permette di aprire il file in una determinata modalità di lettura. La funzione `fgets()` viene utilizzata per leggere ogni riga del file e restituire il suo contenuto. Infine, è importante ricordare di chiudere il file utilizzando la funzione `fclose()` per liberare le risorse del sistema.

## Vedi anche

- [Documentazione ufficiale di PHP sulle funzioni per la gestione dei file](https://www.php.net/manual/en/ref.filesystem.php)
- [Tutorial su come leggere e scrivere file di testo in PHP](https://www.tutorialspoint.com/php/php_files.htm)
- [Utilizzo della libreria di manipolazione dei file di testo in PHP](https://www.geeksforgeeks.org/working-files-php/)