---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:57:52.299240-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
In italiano: "Che cosa e Perché?"

Verificare l'esistenza di una directory permette di sapere se un certo percorso è accessibile o meno. Questo è fondamentale per prevenire errori quando si lavora con file system: leggere, scrivere o modificare solo se la directory è presente.

## How to:
In italiano: "Come Fare:"

Per controllare se una directory esiste in PHP, utilizziamo la funzione `is_dir()`. Ecco un esempio pratico:

```PHP
<?php
$directoryPath = "/path/to/directory";

if (is_dir($directoryPath)) {
    echo "La directory esiste!";
} else {
    echo "La directory non esiste!";
}
?>
```

Output possibile:

```
La directory esiste!
```

o se la directory non esiste:

```
La directory non esiste!
```

## Deep Dive:
In italiano: "Analisi Dettagliata:"

Prima delle versioni attuali, si usavano funzioni come `file_exists()` per verificare l'esistenza delle directory, ma questo non è l'approccio ottimale, poiché `file_exists()` restituisce `true` anche per i file regolari.

Alternativamente, si può combinare `file_exists()` con `is_dir()` per essere ultra specifici:

```PHP
if (file_exists($directoryPath) && is_dir($directoryPath)) {
    // La directory esiste effettivamente come tale.
}
```

Nei sistemi Unix-like, le impostazioni dei permessi possono influire sulla verifica dell'esistenza di una directory; `is_dir()` può restituire `false` se non hai i permessi necessari per vedere la directory.

## See Also:
In italiano: "Vedi Anche:"

- [La documentazione ufficiale di PHP su is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [Filesystem Functions in PHP](https://www.php.net/manual/en/ref.filesystem.php): Una raccolta di funzioni per manipolare il file system.