---
aliases:
- /it/php/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:14.190781-07:00
description: "Verificare se una cartella esiste \xE8 un compito fondamentale nella\
  \ programmazione PHP, poich\xE9 permette di verificare la presenza di una directory\
  \ prima di\u2026"
lastmod: 2024-02-18 23:08:55.984350
model: gpt-4-0125-preview
summary: "Verificare se una cartella esiste \xE8 un compito fondamentale nella programmazione\
  \ PHP, poich\xE9 permette di verificare la presenza di una directory prima di\u2026"
title: Verifica se una directory esiste
---

{{< edit_this_page >}}

## Cosa & Perché?

Verificare se una cartella esiste è un compito fondamentale nella programmazione PHP, poiché permette di verificare la presenza di una directory prima di effettuare operazioni quali leggere o scrivere file al suo interno. Questa operazione aiuta a prevenire errori che potrebbero sorgere tentando di accedere a directory inesistenti ed è essenziale per la gestione dinamica dei file all'interno delle vostre applicazioni.

## Come fare:

Il modo nativo per verificare se una directory esiste in PHP è utilizzando la funzione `is_dir()`. Questa funzione prende un percorso di file come argomento e ritorna `true` se la directory esiste ed è una directory, o `false` in caso contrario.

```php
$directoryPath = "/percorso/alla/tua/directory";

if(is_dir($directoryPath)) {
    echo "La directory esiste.";
} else {
    echo "La directory non esiste.";
}
```

Risultato Esempio:
```
La directory esiste.
```
Oppure, se la directory non esiste:
```
La directory non esiste.
```

Anche se la libreria standard di PHP è sufficientemente robusta per la maggior parte dei compiti di manipolazione di directory e file, talvolta potreste trovarvi nella necessità di una soluzione più comprensiva. Per tali casi, una libreria di terze parti popolare è il componente Filesystem di Symfony. Offre una vasta gamma di utilità per il file system, includendo un modo diretto per verificare se una directory esiste.

Prima di tutto, dovrai installare il componente Filesystem di Symfony. Se stai usando Composer (un gestore di dipendenze per PHP), puoi eseguire il seguente comando nella tua directory di progetto:

```
composer require symfony/filesystem
```

Dopo aver installato il componente Filesystem di Symfony, puoi usarlo per verificare se una directory esiste così:

```php
use Symfony\Component\Filesystem\Filesystem;

$filesystem = new Filesystem();
$directoryPath = '/percorso/alla/tua/directory';

if($filesystem->exists($directoryPath)) {
    echo "La directory esiste.";
} else {
    echo "La directory non esiste.";
}
```

Risultato Esempio:
```
La directory esiste.
```
Oppure, se la directory non esiste:
```
La directory non esiste.
```

Entrambi i metodi forniscono modi affidabili per controllare l'esistenza di una directory in PHP. La scelta tra l'uso delle funzioni incorporate di PHP o una libreria di terze parti come il componente Filesystem di Symfony dipende dalle specifiche esigenze del vostro progetto e se richiedete manipolazioni del file system aggiuntive che potrebbero essere gestite più efficacemente dalla libreria.
