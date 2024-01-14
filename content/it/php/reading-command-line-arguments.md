---
title:                "PHP: Leggere gli argomenti della riga di comando"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Spesso ci troviamo ad avere la necessità di passare informazioni al nostro programma PHP in modo dinamico. Una delle soluzioni più comuni è l'utilizzo dei cosiddetti "command line arguments", ovvero dei parametri passati al programma direttamente dalla linea di comando. In questo post vedremo come leggere e utilizzare questi parametri all'interno del nostro codice PHP.

## Come Fare

Per prima cosa, dobbiamo capire come leggere e accedere ai command line arguments all'interno del nostro script PHP. Per farlo, dobbiamo utilizzare la funzione `$_SERVER['argv']`, che ci restituirà un array contenente tutti i parametri passati al nostro programma. Vediamo un esempio pratico:

```PHP
<?php
// codice per leggere e stampare i command line arguments
$arguments = $_SERVER['argv'];

// stampa il numero totale di argomenti passati
echo "Numero totale di argomenti: " . count($arguments) . "\n";

// stampa tutti gli argomenti passati
foreach ($arguments as $arg) {
    echo $arg . "\n";
}
```

Supponiamo ora di eseguire il nostro script da linea di comando, passando alcuni parametri:

```bash
$ php script.php arg1 arg2 arg3
```

L'output del nostro script sarà il seguente:

```bash
Numero totale di argomenti: 4
script.php
arg1
arg2
arg3
```

Come possiamo notare, il primo elemento dell'array `$_SERVER['argv']` è sempre il nome del nostro script, mentre gli argomenti passati seguono subito dopo.

Oltre a leggere i command line arguments, possiamo anche gestire specifici parametri passati all'interno del nostro script. Ad esempio, supponiamo di dover aggiungere un parametro opzionale che ci permetta di stampare l'argomento passato dopo di esso:

```PHP
<?php
// codice per gestire un parametro opzionale
$arguments = $_SERVER['argv'];

// se il secondo argomento è '--print', stampa il terzo argomento
if ($arguments[1] == '--print') {
    echo "Argomento passato: " . $arguments[2];
} else {
    echo "Nessun parametro opzionale è stato passato.";
}
```

Ora, se eseguiamo il nostro script utilizzando il parametro `--print`:

```bash
$ php script.php arg1 --print arg2
```

L'output sarà il seguente:

```bash
Argomento passato: arg2
```

## Approfondimento

Oltre ai metodi descritti precedentemente, esistono anche altre soluzioni per leggere e gestire i command line arguments all'interno di un programma PHP. Ad esempio, possiamo utilizzare la libreria `Symfony Console`, che ci fornisce una serie di strumenti per gestire in modo più avanzato i parametri passati da linea di comando.

Inoltre, non dimentichiamo che i command line arguments possono anche essere utilizzati per specificare delle opzioni o delle flag all'interno del nostro script, che ci permettono di personalizzare il suo comportamento in base agli input passati.

## Vedi Anche

- [Utilizzo della funzione `$_SERVER['argv']` in PHP](https://www.php.net/manual/en/reserved.variables.argv.php)
- [Symfony Console: Gestire input da linea di comando in PHP](https://symfony.com/doc/current/components/console.html)
- [Passaggio di parametri da linea di comando con PHP](https://www.php.net/manual/en/features.commandline.php)