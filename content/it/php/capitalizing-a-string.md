---
title:                "PHP: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Molte volte quando stiamo scrivendo del codice PHP, abbiamo bisogno di modificare una stringa in modo da farla apparire corretta. Una delle cose più comuni che dobbiamo fare è capitalizzarla, ovvero rendere la prima lettera maiuscola e le altre minuscole. In questo blog post, vedremo come è possibile fare questo in PHP.

## Come fare
Per capitalizzare una stringa in PHP, possiamo utilizzare la funzione `ucfirst()`. Di seguito è riportato un esempio di codice con un input di una stringa e l'output della stessa con la prima lettera maiuscola:

```PHP
$string = "ciao a tutti!";
echo ucfirst($string); // Output: Ciao a tutti!
```

Ma cosa succede se abbiamo una stringa con più di una parola? In questo caso, dobbiamo utilizzare una combinazione di funzioni come `explode()`, `ucfirst()` e `implode()`. Di seguito è riportato un esempio di codice che mostra come capitalizzare ogni parola di una stringa:

```PHP
$string = "ciao a tutti!";
$string_arr = explode(" ", $string); // Convertiamo la stringa in un array separando le parole con uno spazio
$new_string_arr = array(); // Creiamo un nuovo array vuoto
foreach ($string_arr as $word) { // Per ogni parola nell'array
    $capitalized_word = ucfirst($word); // Capitalizziamo la prima lettera di ogni parola
    array_push($new_string_arr, $capitalized_word); // Inseriamo la parola capitalizzata nel nuovo array
}
$new_string = implode(" ", $new_string_arr); // Convertiamo il nuovo array in una stringa unendo le parole con uno spazio
echo $new_string; // Output: Ciao A Tutti!
```

## Approfondimento
Oltre alla funzione `ucfirst()`, esistono altre funzioni utili per manipolare le stringhe in PHP. Ad esempio, `strtolower()` convertirà tutte le lettere di una stringa in minuscolo, mentre `strtoupper()` le convertirà tutte in maiuscolo. Inoltre, se vogliamo capitalizzare una stringa con più di una parola, possiamo utilizzare anche la funzione `ucwords()`.

## Vedi anche
- [Documentazione PHP: Funzioni per le stringhe](https://www.php.net/manual/en/ref.strings.php)
- [Tutorial: Come manipolare le stringhe in PHP](https://www.w3schools.com/php/php_string.asp)
- [Esplora il mondo del PHP con questi tutorial](https://www.codecademy.com/catalog/language/php)