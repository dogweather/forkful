---
title:                "Verifica se una directory esiste"
html_title:           "PHP: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Controllare se una directory esiste è un'operazione comune nella programmazione PHP. Questo ci permette di verificare se una determinata directory è presente sul nostro server e di gestirne i contenuti in modo adeguato. È importante eseguire questo controllo per evitare errori e creare un'esperienza utente più fluida.

## Come fare:
Ecco un esempio di codice PHP che controlla se la directory "immagini" esiste nella cartella corrente:
```PHP
<?php
if (file_exists('immagini')) {
    echo "La directory immagini esiste!";
} else {
    echo "La directory immagini non esiste.";
}
```

Ecco l'output del codice sopra riportato:
```
La directory immagini esiste!
```

## Approfondimento:
Il controllo della presenza di una directory è diventato più facile con l'introduzione di PHP 5. Se utilizzi una versione precedente, potresti usare la funzione `is_dir()` per raggiungere lo stesso risultato. Inoltre, puoi utilizzare la funzione `mkdir()` per creare una nuova directory se quella richiesta non esiste.

## Vedi anche:
- [Funzione `file_exists()` su PHP.net](https://www.php.net/manual/it/function.file-exists.php)
- [Funzione `is_dir()` su PHP.net](https://www.php.net/manual/it/function.is-dir.php)
- [Funzione `mkdir()` su PHP.net](https://www.php.net/manual/it/function.mkdir.php)