---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Leggere gli argomenti della riga di comando stabilisce la comunicazione tra l'utente e lo script. Questo è vitale quando si programmano script che necessitano di input personalizzati per eseguire operazioni dinamiche.

## Come fare:

PHP fornisce un array superglobale ```$argv``` per leggere gli argomenti della riga di comando.

```PHP
<?php
    // stampa il nome dello script
    echo 'Nome script: ' . $argv[0] . "\n";
    
    // stampa il primo argomento
    echo 'Primo argomento: ' . $argv[1] . "\n";
?>
```

Eseguendo `php script.php ciao` si otterrà il seguente output:

```PHP
Nome script: script.php
Primo argomento: ciao
```

Attenzione che `$argv[0]` sarà sempre il nome del tuo script.

## Approfondimento

### Contesto storico

La lettura degli argomenti da riga di comando non è una funzione specifica di PHP; è una funzionalità comune in molti linguaggi di programmazione, risalente alle origini di Unix.

### Alternative

Esiste un'alternativa alla superglobale `$argv` che è la funzione `getopt()`. Questa funzione analizza le opzioni e gli argomenti del comando in base a delle specifiche che si definiscono.

```PHP
<?php
    $options = getopt("a:b:c:");
    print_r($options);
?>
```

### Dettagli implementativi

L'array `$argv` include sempre il nome dello script come primo elemento. Se non si passano argomenti, l'array avrà solo questo elemento; altrimenti, ogni argomento sarà un elemento successivo dell'array.

## Vedi anche

[Manuale PHP: Argomenti della riga di comando](https://www.php.net/manual/it/features.commandline.arguments.php)

[Manuale PHP: getopt()](https://www.php.net/manual/it/function.getopt.php)