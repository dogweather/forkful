---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Trova la lunghezza di una stringa è un'operazione fondamentale in programmazione che ritorna il numero di caratteri in una determinata stringa. Questo è spesso utilizzato dai programmatori per manipolare e controllare i dati, semplificando operazioni come le validazioni di input o le manipolazioni di testo.

## Come fare:

In PHP, troveremo la lunghezza di una stringa utilizzando la funzione incorporata `strlen()`. Questa funzione accetta una stringa come input e ritorna la sua lunghezza.

```PHP
<?php
$stringa = "Ciao, mondo!";
echo strlen($stringa);
?>
```

Il codice sopra produrrà l'output:

```
13
```

## Approfondimenti

Anche se `strlen()` è la funzione più comunemente usata in PHP per trovare la lunghezza di una stringa, esistono alternative. Ad esempio, `mb_strlen()` è un'altra funzione utile se si lavora con stringhe multi-byte, come i caratteri Unicode.

Riguardo i dettagli di implementazione, `strlen()` conta semplicemente il numero di byte in una stringa, non il numero di caratteri. Questo è perché PHP memorizza le stringhe come una sequenza di byte. Perciò, tieni presente che se utilizzi `strlen()` con i caratteri multibyte, potresti non ottenere il risultato atteso.

Infine, rilevante dal punto di vista storico, la funzione `strlen()` è presente dal PHP 4.0.0 e rimane una parte fondamentale del linguaggio PHP.

## Vedi Anche 

Per approfondire su `strlen`: [https://www.php.net/manual/en/function.strlen.php](https://www.php.net/manual/en/function.strlen.php)

Per approfondire su `mb_strlen`: [https://www.php.net/manual/en/function.mb-strlen.php](https://www.php.net/manual/en/function.mb-strlen.php)

Informazioni più dettagliate sulle stringhe in PHP: [https://www.php.net/manual/en/language.types.string.php](https://www.php.net/manual/en/language.types.string.php)