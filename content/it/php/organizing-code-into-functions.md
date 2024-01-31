---
title:                "Organizzazione del codice in funzioni"
date:                  2024-01-26T01:11:39.969310-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizzazione del codice in funzioni"

category:             "PHP"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Organizzare il codice in funzioni significa suddividere il codice in blocchi riutilizzabili con scopi definiti. Lo facciamo per mantenere tutto ordinato, prevenire la ridondanza e rendere il debugging un gioco da ragazzi.

## Come fare:
Immagina di avere del codice ripetitivo per salutare gli utenti. Invece, lo incapsuleremo in una funzione come `greet_user`:

```php
function greet_user($name) {
    return "Ciao, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Output:
```
Ciao, Alice!
Ciao, Bob!
```

Ora, hai a disposizione uno strumento pratico che puoi usare in qualsiasi momento senza dover riscrivere le stesse righe di codice ogni volta che vuoi dire ciao.

## Approfondimento
Le funzioni sono presenti nella programmazione sin dai primi giorni del FORTRAN negli anni '50. Sono una pietra angolare della programmazione strutturata e hanno a che fare con la modularità e l'isolamento. Alternative? Beh, puoi orientarti verso la programmazione orientata agli oggetti e parlare di classi e metodi, che sono funzioni con un vestito elegante. Per quanto riguarda PHP, i dettagli dell'implementazione includono la specificazione di valori predefiniti per i parametri, il type hinting per gli input e la possibilità di restituire valori multipli utilizzando un array o, a partire da PHP 7.1, una lista.

Ecco una svolta moderna con dichiarazione di tipo e valori predefiniti:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 ha introdotto anche le funzioni arrow, utili per scrivere funzioni concise su una sola riga, comunemente usate nelle operazioni di array:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Output:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Vedi Anche
- [Manuale di PHP sulle Funzioni](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: Il Modo Giusto - Funzioni](https://phptherightway.com/#functions)
- [Scopri le Funzioni Arrow di PHP 7.4](https://stitcher.io/blog/short-closures-in-php)
