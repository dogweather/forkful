---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La ricerca e sostituzione del testo è il procedimento con il quale un testo specifico viene individuato ("ricerca") e cambiato ("sostituito") con un altro. I programmatori la utilizzano per manipolare i dati, correggere gli errori e personalizzare l'output.

## Come Fai:
Per sostituire il testo in PHP, utilizziamo la funzione `str_replace()`. Esempio:

```PHP
<?php
$frase = "Mi piace la pizza";
$nova_frase = str_replace("pizza", "pasta", $frase);

echo $nova_frase;
?>
```
Output:
```
Mi piace la pasta
```
In questo esempio, abbiamo sostituito "pizza" con "pasta".

## Approfondiamo:
La funzione `str_replace()` è incorporata in PHP dalla sua versione 4.0.2. Un'alternativa è usare funzioni regular expression come `preg_replace()` che offrono più flessibilità, ma sono più complesse.

Quando `str_replace()` viene eseguito, PHP cerca la corrispondenza del testo in modo sequenziale e sostituisce tutte le occorrenze trovate. È case-sensitive di default, cioè distingue tra maiuscole e minuscole. Per una ricerca senza distinzione tra maiuscole e minuscole, utilizza `str_ireplace()`.

## Vedi Anche:
3. [StackOverflow - When to use preg_replace?](https://stackoverflow.com/questions/6723591/preg-replace-vs-str-replace)