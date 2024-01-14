---
title:    "PHP: Lettura degli argomenti dalla riga di comando"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

La lettura degli argomenti della riga di comando è una skill essenziale per ogni programmatore PHP. Conoscere come farlo ti permetterà di creare script più flessibili e interattivi, in grado di accettare input utente e adattarsi a diverse situazioni. Continua a leggere per scoprire come!

## Come Fare

Per leggere gli argomenti della riga di comando in PHP, utilizziamo la funzione incorporata `getopt()`. Questa funzione prende due parametri obbligatori: una stringa contenente le opzioni disponibili e un array contenente i valori degli argomenti. Ecco un esempio di codice:

```PHP
$opzioni = "a:b:";
$valori = getopt($opzioni);

echo "Primo argomento: " . $valori['a'] . "\n";
echo "Secondo argomento: " . $valori['b'] . "\n";
```

La stringa `$opzioni` specifica due opzioni (`a` e `b`) seguite da `:` per indicare che richiedono un valore. Ora eseguiamo lo script fornendo questi argomenti:

```
$ php script.php -a valore1 -b valore2
```

L'output sarà:

```
Primo argomento: valore1
Secondo argomento: valore2
```

## Approfondimento

Oltre alle opzioni, la funzione `getopt()` ci permette di leggere anche gli argomenti posizionali. Ad esempio, se vogliamo leggere un elenco di file forniti come argomenti, possiamo utilizzare questa sintassi:

```PHP
$opzioni = "f:";
$valori = getopt($opzioni);

$files = $argv;

//rimuoviamo il nome dello script dal file
array_shift($files);

foreach($files as $file){
    echo "Nome file: " . $file . "\n";
}
```

Eseguendo lo script in questo modo:

```
$ php script.php -f file1.txt file2.txt file3.txt
```

Avremo questo output:

```
Nome file: file1.txt
Nome file: file2.txt
Nome file: file3.txt
```

È importante notare che i flag delle opzioni possono essere combinati, quindi possiamo usare qualcosa come `-ab` per specificare entrambe le opzioni con un solo argomento.

## Vedi Anche

- [Documentazione PHP su getopt()](https://www.php.net/manual/en/function.getenv.php)
- [Tutorial su CLI PHP su Codecademy](https://www.codecademy.com/learn/learn-php/modules/php-cli)