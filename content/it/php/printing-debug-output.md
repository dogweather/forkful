---
title:    "PHP: Stampa dell'output di debug"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

In questa guida, parleremo di una pratica comune nella programmazione PHP: la stampa di output di debug. Scopriremo perché è importante e come è possibile utilizzarla efficacemente per risolvere i problemi nei tuoi script.

## Come fare

Per stampare output di debug in PHP, puoi utilizzare la funzione `print_r()` o `var_dump()`. Queste funzioni sono molto utili perché mostrano i valori delle variabili e degli array in modo molto dettagliato. Ecco un esempio di come utilizzarle:

```PHP
$numero = 10;
$array = [6, 8, 12];

print_r($numero);
// Output: 10

var_dump($array);
/* Output:
array(3) {
  [0]=>
  int(6)
  [1]=>
  int(8)
  [2]=>
  int(12)
}
*/
```

Come puoi vedere, `print_r()` stampa semplicemente il valore della variabile, mentre `var_dump()` mostra tutte le informazioni sulla variabile, inclusi il tipo e la lunghezza dei dati.

Puoi anche usare `echo` per stampare testo e variabili in una singola riga, ma ricorda di utilizzare `var_export()` per visualizzare correttamente i valori di array e oggetti. Ad esempio:

```PHP
$nome = "Maria";
$anni = 28;

echo "Il tuo nome è " . $nome . " e hai " . $anni . " anni";

// Output: Il tuo nome è Maria e hai 28 anni

$array = ["uno", "due", "tre"];

echo "Array: " . var_export($array, true);
/* Output: Array: array (
  0 => 'uno',
  1 => 'due',
  2 => 'tre',
) */
```

## Approfondimento

Ora che hai imparato a utilizzare la stampa di output di debug, è importante ricordare che questa pratica dovrebbe essere utilizzata solo durante la fase di sviluppo del tuo script. È sconsigliato includere il codice di debug in produzione, poiché può rivelare informazioni sensibili agli utenti finali e rallentare il funzionamento del sito.

Inoltre, puoi anche utilizzare strumenti di debug avanzati come Xdebug per ottenere molto più dettagli sulle variabili e sulle tracce di esecuzione del codice. Questi strumenti sono estremamente utili durante la risoluzione di bug più complessi ed è consigliato imparare ad utilizzarli.

## Vedi anche

- [PHP Debugging Tips and Tricks](https://www.php.net/manual/en/debugging-tips.php)
- [Mastering PHP Debugging with Xdebug](https://code.tutsplus.com/tutorials/mastering-php-debugging-with-xdebug--net-36398)
- [Debugging in PHP](https://www.w3schools.com/php/php_debugging.asp)
- [Xdebug Documentation](https://xdebug.org/docs/)