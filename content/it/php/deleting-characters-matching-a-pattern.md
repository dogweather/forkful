---
title:                "Eliminare caratteri corrispondenti a uno schema"
html_title:           "PHP: Eliminare caratteri corrispondenti a uno schema"
simple_title:         "Eliminare caratteri corrispondenti a uno schema"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché eliminare caratteri che corrispondono a un pattern
Eliminare caratteri che corrispondono a un pattern è utile quando si lavora con stringhe di testo e si vuole rimuovere elementi indesiderati. Ad esempio, si potrebbe voler rimuovere tutte le vocali da una stringa per rendere la lettura più facile o eliminare spazi bianchi inutili.

## Come fare
Il modo più semplice per eliminare caratteri che corrispondono a un pattern in PHP è utilizzare la funzione [preg_replace](https://www.php.net/manual/en/function.preg-replace.php). Questa funzione consente di sostituire una o più occorrenze di un pattern all'interno di una stringa con un altro valore.

Ecco un esempio di codice che elimina tutte le vocali da una stringa utilizzando la funzione `preg_replace`:

```PHP
$stringa = "Ciao mondo!";

$risultato = preg_replace("/[aeiou]/i", "", $stringa);

echo $risultato;
// Output: C mnd!
```

Nell'esempio sopra, il primo parametro della funzione `preg_replace` è il pattern che corrisponde alle vocali, mentre il secondo parametro è il valore che le sostituirà (in questo caso una stringa vuota). Il terzo parametro è la stringa originale su cui applicare la sostituzione.

## Approfondimento
La funzione `preg_replace` utilizza le cosiddette espressioni regolari per definire il pattern da cercare nella stringa. Queste espressioni regolari consistono in una serie di caratteri e operatori che permettono di definire con precisione quali caratteri si vogliono cercare e sostituire.

Per esempio, nel codice sopra abbiamo utilizzato il pattern `/[aeiou]/i` dove le lettere tra parentesi quadre indicano i caratteri che vogliamo trovare (in questo caso tutte le vocali) e il flag `i` indica una ricerca case-insensitive, cioè non fa distinzione tra maiuscole e minuscole.

Per imparare di più sulle espressioni regolari in PHP, si consiglia di leggere la [documentazione ufficiale](https://www.php.net/manual/en/pcre.pattern.php) o fare dei tutorial specifici.

## Vedi anche
- [La funzione preg_replace nella documentazione di PHP](https://www.php.net/manual/en/function.preg-replace.php)
- [Tutorial sulle espressioni regolari in PHP](https://www.regular-expressions.info/php.html)
- [La classe Regex di PHP per un utilizzo avanzato delle espressioni regolari](https://www.php.net/manual/en/class.regex.php)