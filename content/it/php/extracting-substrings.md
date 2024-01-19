---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?

L'estrazione di sottocatene è l'atto di ottenere una piccola porzione di una stringa più grande. Utilizziamo questa tecnica quando vogliamo concentrarci su specifici dati all'interno di una stringa più grande.

## Come fare:

Puoi estrarre sottocatene in PHP utilizzando la funzione `substr`:

```PHP
<?php
$stringa = "Ciao, mondo!";
echo substr($stringa, 0, 4);  //output: "Ciao"
?>
```

In questa espressione, il primo parametro della funzione `substr` è la stringa da cui stai prelevando, il secondo è l'indice da cui iniziare (inizia da 0), e il terzo è la lunghezza della sottocatena che desideri ottenere. 

## Approfondimento

Mentre `substr` è la funzione più comune per estrarre sottocatene in PHP, esistono alternative che potrebbero essere più appropriate a seconda del tuo caso d'uso.

Per esempio:
- `strstr` se conosci la sottocatena che stai cercando.
- `strpos` e `strrpos` se vuoi solo trovare la posizione del primo o dell'ultimo esempio di una sottocatena.

L'estrazione di sottocatene viene utilizzata in PHP sin dalla sua creazione negli anni '90. Come molte altre funzioni di stringa in PHP, `substr` è basata su funzionalità simili presenti nelle librerie C standard, rendendo più semplice per i programmatori con esperienza C imparare PHP.

In termini di implementazione, `substr` in PHP può gestire stringhe contenenti caratteri multi-byte (come l'utf-8), rendendola adatta per lavorare con una vasta gamma di lingue e dataset.

## Vedi Anche

Per approfondimenti sull'estrazione di sottocatene e altre funzioni di manipolazione delle stringhe in PHP, consulta le seguenti risorse:
- [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
- [PHP: strstr - Manual](https://www.php.net/manual/en/function.strstr.php)
- [PHP: strpos - Manual](https://www.php.net/manual/en/function.strpos.php)
- [PHP: strrpos - Manual](https://www.php.net/manual/en/function.strrpos.php)