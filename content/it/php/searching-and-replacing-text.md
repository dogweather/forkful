---
title:                "PHP: Ricerca e sostituzione di testo"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
Il processo di ricerca e sostituzione di testo è un'abilità fondamentale per tutti i programmatori PHP. È un modo rapido e efficiente per modificare grandi quantità di testo in modo coerente all'interno del codice.

## Come fare
Per effettuare una ricerca e sostituzione di testo in PHP, è possibile utilizzare la funzione `str_replace()`. Prende in input tre parametri: il testo da cercare, il testo con cui sostituirlo e la stringa su cui deve essere eseguita l'operazione.

```PHP
<?php
// Creazione di una stringa di esempio
$stringa = "Questo è un esempio di ricerca e sostituzione di testo in PHP.";

// Utilizzo della funzione str_replace per sostituire "esempio" con "esempio pratico" all'interno della stringa
$result = str_replace("esempio", "esempio pratico", $stringa);

// Stampa del risultato
echo $result; // Questo è un esempio pratico di ricerca e sostituzione di testo in PHP.
?>
```

## Approfondimento
Esistono diverse varianti della funzione `str_replace()`, tra cui `str_ireplace()` che fa la stessa operazione di ricerca e sostituzione ma ignorando la differenza tra maiuscole e minuscole, e `preg_replace()` che utilizza espressioni regolari per effettuare sostituzioni più complesse. Inoltre, è importante notare che la funzione `str_replace()` è case-sensitive, quindi è necessario prestare attenzione alle maiuscole e minuscole quando si utilizza.

## Vedi anche
- Documentazione ufficiale di PHP per la funzione str_replace: https://www.php.net/manual/en/function.str-replace.php
- Tutorial su come utilizzare le espressioni regolari in PHP: https://www.w3schools.com/php/php_regex.asp
- Esempi di sostituzione di testo con l'utilizzo della funzione preg_replace: https://www.php.net/manual/en/function.preg-replace.php