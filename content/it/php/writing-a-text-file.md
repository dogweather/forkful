---
title:                "PHP: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'attività comune per i programmatori e potrebbe sembrare banale. Tuttavia, il processo di scrittura di un file di testo può essere estremamente utile per salvare dati o creare report in modo automatizzato.

## Come fare

Per scrivere un file di testo in PHP, è necessario utilizzare la funzione `file_put_contents()`, passando come primo parametro il percorso del file e come secondo parametro il contenuto che si desidera scrivere.

```PHP
$file_path = "test.txt";
$content = "Questo è un esempio di testo scritto con PHP.";

file_put_contents($file_path, $content);
```

Se il file specificato non esiste, verrà creato automaticamente. Se invece il file esiste già, il contenuto verrà sovrascritto. È anche possibile specificare un terzo parametro opzionale per specificare la modalità di scrittura del file (ad esempio, appendere il contenuto alla fine del file anziché sovrascrivere).

## Approfondimento

Per scrivere un file di testo in modo più avanzato, è possibile utilizzare la classe `SplFileObject`. Questa classe permette di gestire la scrittura del file tramite metodi come `fwrite()` e di leggere il contenuto del file tramite il metodo `fread()`.

Ecco un esempio di utilizzo della classe `SplFileObject` per scrivere e leggere un file di testo:

```PHP
$file_path = "test.txt";
$file = new SplFileObject($file_path, "w+");

// Scrive il contenuto nel file
$file->fwrite("Questo è un esempio di testo scritto con PHP.");

// Imposta il puntatore all'inizio del file
$file->rewind();

// Legge il contenuto dal file
$content = $file->fread($file->getSize());

echo $content; // Stampa il contenuto del file
```

## Vedi anche

- [Funzione file_put_contents() di PHP](https://www.php.net/manual/en/function.file-put-contents.php)
- [Classe SplFileObject di PHP](https://www.php.net/manual/en/class.splfileobject.php)