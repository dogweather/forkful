---
title:                "Creazione di un file temporaneo"
html_title:           "PHP: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché creare un file temporaneo?

I file temporanei sono utilizzati per una varietà di scopi nella programmazione PHP. Possono essere utilizzati per archiviare dati temporaneamente durante l'esecuzione di uno script o per eseguire operazioni su file senza doverli salvare permanentemente. In generale, i file temporanei sono utili quando si desidera manipolare o archiviare dati in modo temporaneo, senza dover creare un file definitivo.

## Come creare un file temporaneo in PHP

Per creare un file temporaneo in PHP, possiamo utilizzare la funzione `tmpfile()`. Questa funzione crea un file vuoto all'interno della directory temporanea del sistema operativo e restituisce un handler al file. Possiamo quindi utilizzare questo handler per scrivere i dati nel file temporaneo. 

Ecco un esempio di codice per creare e scrivere sul file temporaneo:

```
<?php
$file = tmpfile(); //crea un file temporaneo
fwrite($file, "Questo è un file temporaneo!"); //scrive dati nel file
$filename = stream_get_meta_data($file)['uri']; //recupera il nome del file temporaneo
echo "File temporaneo creato con successo: $filename";
fclose($file); //chiude e elimina il file temporaneo
```

Esempio di output:

```
File temporaneo creato con successo: /tmp/php7OXaBr
```

## Approfondimento
Oltre alla funzione `tmpfile()`, esistono altri metodi per creare file temporanei in PHP, come ad esempio l'utilizzo di `sys_get_temp_dir()` per ottenere la directory temporanea del sistema, oppure l'utilizzo della funzione `tempnam()` per creare un file temporaneo con un nome specificato.

Inoltre, è importante ricordare di eliminare i file temporanei una volta che non sono più necessari, per evitare di appesantire il sistema e di avere file inutilizzati. Possiamo utilizzare la funzione `unlink()` per eliminare un file temporaneo.

## Vedi anche
- PHP.net - tmpfile(): https://www.php.net/manual/en/function.tmpfile.php
- PHP.net - sys_get_temp_dir(): https://www.php.net/manual/en/function.sys-get-temp-dir.php
- PHP.net - tempnam(): https://www.php.net/manual/en/function.tempnam.php