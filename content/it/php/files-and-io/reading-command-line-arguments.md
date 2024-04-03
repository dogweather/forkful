---
date: 2024-01-20 17:56:23.713537-07:00
description: 'How to: - Come fare: .'
lastmod: '2024-03-13T22:44:43.531975-06:00'
model: gpt-4-1106-preview
summary: .
title: Lettura degli argomenti della riga di comando
weight: 23
---

## How to: - Come fare:
```PHP
<?php
// Verifica se ci sono argomenti passati
if ($argc > 1) {
    // Stampa ogni argomento passato
    for ($i = 1; $i < $argc; $i++) {
        echo "Argomento $i: " . $argv[$i] . "\n";
    }
} else {
    echo "Nessun argomento passato.";
}

// Esempio di utilizzo:
// $ php script.php arg1 arg2 arg3

// Output previsto:
// Argomento 1: arg1
// Argomento 2: arg2
// Argomento 3: arg3
?>
```

## Deep Dive - In Profondità
La lettura degli argomenti dalla linea di comando è una pratica vecchia quanto la programmazione stessa. PHP, nato come linguaggio per il web, ha esteso le sue capacità alla CLI nella versione 4.3.0. 

I due superglobali `$argc` (argument count) e `$argv` (argument values) sono il cuore di questa funzionalità. `$argc` conta gli argomenti, mentre `$argv` è un array che li contiene. Ricorda che `$argv[0]` è sempre il nome dello script.

Esistono alternative come `getopt()` per opzioni più complesse, ma `$argc` e `$argv` sono perfetti per la semplicità. Sotto il cofano, PHP utilizza le stesse strutture dati disponibili nel C, il che garantisce rapidità ed efficienza.

## See Also - Vedi Anche
- [PHP Command line usage](https://www.php.net/manual/en/features.commandline.php) - Documentazione ufficiale PHP sull'uso della linea di comando.
- [getopt](https://www.php.net/manual/en/function.getopt.php) - Documentazione sulla funzione 'getopt' per gestire le opzioni più avanzate della linea di comando.
