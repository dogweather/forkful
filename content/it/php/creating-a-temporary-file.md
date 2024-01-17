---
title:                "Creare un file temporaneo"
html_title:           "PHP: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Creare un file temporaneo è una pratica comune tra i programmatori PHP. Si tratta di creare un file che contiene dati temporanei, che verranno utilizzati e poi eliminati durante l'esecuzione del programma. Questo è spesso fatto per gestire dati o informazioni che non hanno bisogno di essere conservati permanentemente.

## Come fare:

```php 
$temp_file = tmpfile(); 
fwrite($temp_file, "Questo è un file temporaneo."); 
$contents = fread($temp_file, filesize($temp_file)); 
echo $contents;

// Output: Questo è un file temporaneo.
```

Il codice sopra crea un file temporaneo utilizzando la funzione PHP `tmpfile()`. Questa funzione restituisce un puntatore al file temporaneo creato, che può poi essere utilizzato per scrivere o leggere dati tramite le funzioni `fwrite()` e `fread()`. Una volta che il programma termina, il file temporaneo verrà eliminato automaticamente.

## Approfondimenti:

La creazione di file temporanei è una pratica comune non solo in PHP, ma anche in altri linguaggi di programmazione. È spesso utilizzata per gestire dati sensibili o per evitare l'utilizzo di risorse di sistema inutile.

Un'alternativa alla creazione di file temporanei è l'utilizzo di variabili temporanee. Tuttavia, questa pratica può portare a problemi di sicurezza e rischi di sovrascrittura dei dati.

Per quanto riguarda l'implementazione dei file temporanei in PHP, è importante conoscere le impostazioni del server che possono influenzare la loro creazione e gestione. Ad esempio, la funzione `tmpfile()` può non funzionare se l'estensione PHP `sysvshm` non è abilitata nel server.

## Vedi anche:

- [Documentazione ufficiale di PHP su tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [Articolo su quando e come utilizzare file temporanei in PHP](https://www.cloudways.com/blog/temporary-files-in-php/)