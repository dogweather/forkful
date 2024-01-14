---
title:    "PHP: Eliminazione dei caratteri corrispondenti a un modello"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti trovare la necessità di eliminare dei caratteri che corrispondono ad un determinato modello all'interno del tuo codice PHP. Forse stai cercando di validare l'input degli utenti, oppure vuoi rimuovere caratteri invisibili che possono causare errori. Qualunque sia il motivo, esiste un modo semplice per farlo utilizzando il linguaggio di programmazione PHP.

## Come fare

Per eliminare i caratteri che corrispondono ad un modello, è necessario utilizzare la funzione `preg_replace()` in PHP. Questa funzione accetta tre parametri: il modello di ricerca, il nuovo valore di sostituzione e la stringa da cui eliminare i caratteri. Per esempio, se volessimo eliminare tutti i numeri da una stringa, il codice sarebbe il seguente:

```PHP
<?php
$stringa = "Hello123World";
$stringa_nuova = preg_replace("/[0-9]/", "", $stringa);
echo $stringa_nuova; //Stampa "HelloWorld"
?>
```
In questo esempio, abbiamo utilizzato il modello `/[0-9]/` che indica tutti i numeri da 0 a 9. Tutti i caratteri che corrispondono a questo modello verranno sostituiti con una stringa vuota, effettivamente eliminandoli dalla stringa originale.

È possibile utilizzare anche espressioni regolari più complesse per eliminare caratteri specifici o intere parole. Ad esempio, se volessimo eliminare tutte le vocali da una stringa, potremmo utilizzare il modello `/[aeiou]/i`, dove il flag `i` indica che la ricerca deve essere case-insensitive. 

## Approfondimento

Se stai cercando di eliminare caratteri da una stringa che può contenere anche caratteri speciali, come ad esempio un URL, è importante utilizzare la funzione `preg_quote()` per evitare errori.

Ecco un esempio di come utilizzarla:

```PHP
<?php
$stringa = "https://www.example.com/?var=foo&bar=baz";
$caratteri_speciali = ["+", "=", "&"];
$stringa_nuova = preg_replace("/[" . preg_quote(implode($caratteri_speciali)) . "]/", "", $stringa);
echo $stringa_nuova; //Stampa "httpswwwexample.com?varfoobarbaz"
?>
```

In questo caso, abbiamo utilizzato la funzione `preg_quote()` per indicare a `preg_replace()` di non considerare i caratteri speciali indicati all'interno dell'espressione regolare.

## Vedi anche

- [Documentazione PHP: La funzione preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Esempi di espressioni regolari in PHP](https://www.w3schools.com/php/php_regex.asp)
- [Validazione input degli utenti in PHP](https://www.w3schools.com/php/php_form_validation.asp)