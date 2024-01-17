---
title:                "Ricerca e sostituzione di testo"
html_title:           "PHP: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La ricerca e la sostituzione di testo è una pratica comune nella programmazione, in cui si cerca un determinato stringa di testo all'interno di un file o di una stringa e si sostituisce con un'altra stringa di testo. I programmatori lo fanno per effettuare modifiche rapide e consistenti nei loro codici o per automatizzare il processo di aggiornamento di informazioni all'interno di un database.

## Come fare:

Per eseguire la ricerca e la sostituzione di testo in PHP, è possibile utilizzare la funzione **str_replace()**. Questa funzione accetta tre parametri: la stringa da cercare, la stringa con cui sostituire e la stringa o l'array in cui effettuare la ricerca. Ad esempio:

```
<?php

$stringa = "Ciao amici, come state?";
// La seguente riga sostituisce "Ciao" con "Benvenuti"
echo str_replace("Ciao", "Benvenuti", $stringa);

?>
```

L'output di questo codice sarà **Benvenuti amici, come state?**

È possibile anche utilizzare la funzione **preg_replace()** per eseguire la ricerca e la sostituzione utilizzando espressioni regolari. Ad esempio:

```
<?php

$stringa = "addio dimmi addio";
// La seguente riga sostituisce tutte le occorrenze di "addio" con "ciao"
echo preg_replace("/addio/", "ciao", $stringa);

?>
```

L'output di questo codice sarà **ciao dimmi ciao**

## Approfondimento:

La ricerca e la sostituzione di testo è una tecnica molto utile e comune nella programmazione. In passato, i programmatori dovevano effettuare questa operazione manualmente, modificando ogni occorrenza della stringa desiderata. Tuttavia, grazie alle funzioni **str_replace()** e **preg_replace()** in PHP, questo processo può essere automatizzato e reso molto più efficiente.

Inoltre, è possibile utilizzare anche la funzione **str_ireplace()** per effettuare una ricerca e una sostituzione case-insensitive, così come la funzione **preg_replace_callback()** per sostituire il testo in base a una funzione di callback.

Alcune alternative alla ricerca e alla sostituzione di testo in PHP includono l'utilizzo di linguaggi di scripting come JavaScript o Python, o l'utilizzo di strumenti di terze parti come sed o awk.

## Vedi anche:

- La documentazione ufficiale di PHP sulla funzione [str_replace()](https://www.php.net/manual/it/function.str-replace.php)
- La documentazione ufficiale di PHP sulla funzione [preg_replace()](https://www.php.net/manual/it/function.preg-replace.php)