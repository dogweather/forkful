---
date: 2024-01-20 17:54:45.586248-07:00
description: 'How to: (Come fare:) Risultato esemplificativo.'
lastmod: '2024-04-05T21:53:44.298079-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) Risultato esemplificativo.
title: Lettura di un file di testo
weight: 22
---

## How to: (Come fare:)
```PHP
<?php
// Aprire un file in sola lettura
$handle = fopen("testo_esempio.txt", "r");

// Controllare se il file è stato aperto con successo
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        // Processare la linea letta
        echo $line;
    }
    // Chiudere il file handle
    fclose($handle);
} else {
    // Errore nell'apertura del file
    echo "Errore nell'apertura del file.";
}
?>
```
Risultato esemplificativo:
```
Questo è il contenuto del file.
Ogni riga è letta una alla volta.
```

## Deep Dive (Approfondimento)
Aprire e leggere i file di testo è una pratica fondamentale nella programmazione PHP, risalente alle sue origini come script per processare e accedere a dati lato server. Alternativamente, si possono utilizzare le funzioni `file_get_contents()` o `file()` per leggere interi file. Dettaglio importante: assicurarsi di gestire sempre le permissions e di evitare vulnerabilità, come quelle agli attacchi di tipo Path Traversal.

## See Also (Vedi Anche)
- [PHP Official Documentation on Reading Files](https://www.php.net/manual/en/function.fopen.php)
- [W3Schools PHP File Open/Read/Close](https://www.w3schools.com/php/php_file.asp)
- [Handling File Uploads in PHP](https://www.php.net/manual/en/features.file-upload.php)
