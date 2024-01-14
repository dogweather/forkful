---
title:    "PHP: Trovare la lunghezza di una stringa"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione comune nella programmazione PHP. Conoscere e utilizzare questa funzione può rendere il tuo codice più efficiente e maneggevole.

## Come fare

Per trovare la lunghezza di una stringa in PHP, puoi utilizzare la funzione `strlen()`. Questa funzione accetta una stringa come parametro e restituisce il numero di caratteri all'interno della stringa. Ecco un esempio di codice:

```PHP
$my_string = "Ciao a tutti!";
echo strlen($my_string);

// Output: 13
```

In questo esempio, la stringa "Ciao a tutti!" ha una lunghezza di 13 caratteri e viene stampata a schermo. Puoi anche utilizzare la funzione `strlen()` per controllare la lunghezza di una variabile stringa dinamicamente, ad esempio in un loop.

## Approfondimento

La funzione `strlen()` conta i caratteri all'interno di una stringa in modo sensibile al caso, il che significa che anche le lettere maiuscole e minuscole sono considerate come caratteri separati. Inoltre, i caratteri speciali e gli spazi sono inclusi nella conteggio della lunghezza. Questa funzione è molto utile per la manipolazione di stringhe, ad esempio per validare la lunghezza di una password o controllare che un input utente non superi una certa lunghezza.

## Vedi anche

- Documentazione ufficiale di PHP `strlen()`: https://www.php.net/manual/en/function.strlen.php
- Esempi di utilizzo di `strlen()`: https://www.w3schools.com/php/func_string_strlen.asp
- Tutorial sulle operazioni di base sulle stringhe in PHP: https://www.hostinger.it/tutorial/php-string/
- Video di spiegazione sulla funzione `strlen()`: https://www.youtube.com/watch?v=D1g-9Pg6HZw&ab_channel=FisrtStepsWebDevelopment