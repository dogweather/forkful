---
title:                "Lettura degli argomenti della linea di comando"
html_title:           "PHP: Lettura degli argomenti della linea di comando"
simple_title:         "Lettura degli argomenti della linea di comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere gli argomenti della riga di comando è il processo di acquisire gli input specificati quando si avvia un programma da riga di comando. I programmatori spesso lo fanno per consentire agli utenti di personalizzare l'esecuzione del programma.

## Come fare:
```PHP
//Esempio di lettura di argomenti dalla riga di comando
// php script.php arg1 arg2

$args = $argv; //Salva gli argomenti in una variabile
echo "Primo argomento: " . $args[1]; //Output: Primo argomento: arg1
echo "Secondo argomento: " . $args[2]; //Output: Secondo argomento: arg2
```

## Approfondimento:
La lettura degli argomenti della riga di comando è un'azione comune nei linguaggi di programmazione da riga di comando come PHP, ma è meno utilizzata nei linguaggi di programmazione orientati agli oggetti come Java. Gli sviluppatori possono scegliere di utilizzare un framework come Symfony o Laravel per semplificare la gestione degli argomenti della riga di comando.

## Vedi anche:
Altre risorse utili sul linguaggio PHP e sulla gestione degli argomenti della riga di comando:
- [PHP: Argv](https://www.php.net/manual/en/reserved.variables.argv.php)
- [Symfony Console Component](https://symfony.com/doc/current/components/console.html)
- [Laravel Console Commands](https://laravel.com/docs/8.x/artisan)