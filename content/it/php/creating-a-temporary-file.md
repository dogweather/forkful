---
title:    "PHP: Creazione di un file temporaneo"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché creare un file temporaneo in PHP

Creare un file temporaneo in PHP può essere utile per una varietà di ragioni. Ad esempio, potresti voler archiviare dati temporanei durante l'esecuzione di uno script, o creare un file temporaneo per eseguire alcune operazioni e poi eliminarlo. Inoltre, i file temporanei possono essere utili per gestire risorse di sistema come database o cache.

## Come creare un file temporaneo in PHP

Per creare un file temporaneo in PHP, possiamo utilizzare la funzione `tempnam ()`, che accetta due parametri: il percorso della directory in cui creare il file e un prefisso opzionale per il nome del file. Ad esempio:

```PHP
$temp_file = tempnam("/tmp", "prefix_");
echo "Il file temporaneo creato è: " . $temp_file;
```

Questo codice creerà un file temporaneo con il prefisso "prefix_" nella directory "/tmp" e restituirà il percorso completo del file appena creato.

## Approfondimento sulla creazione di file temporanei in PHP

Il file temporaneo creato con la funzione `tempnam ()` è un file vuoto che può essere utilizzato per salvare dati temporanei. Tuttavia, è importante notare che questo file non viene automaticamente eliminato dopo l'esecuzione dello script. Per eliminare il file temporaneo, è necessario utilizzare la funzione `unlink ()`.

Inoltre, è possibile specificare una directory diversa da "/tmp" per la creazione del file temporaneo utilizzando la costante predefinita `sys_get_temp_dir ()`. Questa costante restituirà il percorso della directory temporanea di sistema predefinita sulla piattaforma in uso.

## See Also

- Documentazione PHP su [tempnam()](https://www.php.net/manual/it/function.tempnam.php)
- Documentazione PHP su [unlink()](https://www.php.net/manual/it/function.unlink.php)
- Articolo su [gestione dei file temporanei in PHP](https://www.html.it/pag/4423/gestione-dei-file-temporanei-in-php/)