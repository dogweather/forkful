---
title:    "PHP: Cercare e Sostituire Testo"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
La ricerca e la sostituzione di testo è una delle funzioni fondamentali della programmazione PHP. Ti permette di effettuare rapidamente modifiche su un ampio numero di stringhe all'interno del tuo codice. Se sei un programmatore PHP, sicuramente ti troverai in situazioni in cui avrai bisogno di utilizzare questa funzione.

## Come
```PHP
// Esempio di ricerca e sostituzione
$stringa = "Benvenuto nel mio blog!";
echo str_replace("Benvenuto", "Ciao", $stringa);
```
Questo codice produrrà l'output "Ciao nel mio blog!". Come puoi vedere, la funzione `str_replace` prende come argomenti il testo da cercare, il testo con cui sostituirlo e la stringa su cui effettuare la modifica.

## Deep Dive
Oltre alla funzione `str_replace`, esistono anche altre funzioni utili per la ricerca e la sostituzione di testo. Ad esempio, `str_ireplace` esegue la stessa operazione ma ignorando la differenza tra maiuscole e minuscole. Inoltre, puoi utilizzare le espressioni regolari per effettuare ricerche e sostituzioni più complesse.

## Vedi anche
- [Documentazione PHP su str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [Esempi di utilizzo delle espressioni regolari in PHP](https://www.php.net/manual/en/function.preg-replace.php)