---
title:                "Unire stringhe"
html_title:           "PHP: Unire stringhe"
simple_title:         "Unire stringhe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Concatenare le stringhe in informatica significa unire più stringhe di testo per crearne una sola. I programmatori lo fanno spesso per creare un'unica stringa che contenga informazioni diverse e per facilitare la gestione dei dati. Questo è molto utile quando si lavora con grandi quantità di testo o quando si deve formattare un output specifico.

## Come fare:
```PHP
$stringa1 = "Ciao";
$stringa2 = "mi";
$stringa3 = "chiamo";
$stringa4 = "Marco";

echo $stringa1 . " " . $stringa2 . " " . $stringa3 . " " . $stringa4;
```
Output: Ciao mi chiamo Marco

## Approfondimento:
La concatenazione di stringhe è stata introdotta per la prima volta nella versione PHP 4 e da allora è diventata una funzione fondamentale nella gestione dei dati. Un'alternativa alla concatenazione di stringhe è l'utilizzo di variabili con una sintassi dinamica, ma ciò richiede più codice e può essere meno efficiente in alcune situazioni. Per implementare la concatenazione di stringhe, PHP utilizza il carattere di concatenazione "." e permette di unire le stringhe in modo rapido e semplice.

## Vedi anche:
- [Documentazione ufficiale su stringhe in PHP] (https://www.php.net/manual/en/language.types.string.php)
- [Tutorial su come usare la concatenazione di stringhe] (https://www.w3schools.com/php/php_operators.asp)
- [Video tutorial sulla concatenazione di stringhe] (https://www.youtube.com/watch?v=O4z-B9KbdU0)