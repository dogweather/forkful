---
title:                "Estrazione di sottostringhe"
html_title:           "PHP: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
L'estrazione di sottostringhe (o substrings in inglese) si riferisce all'atto di ottenere una sequenza di caratteri più piccola da una stringa più ampia. I programmatori spesso lo fanno per manipolare e analizzare le stringhe in modo più efficiente.

## Come fare:
Estraiamo una sottostringa utilizzando la funzione `substr()` di PHP. Possiamo specificare la stringa iniziale e il numero di caratteri da estrarre come argomenti della funzione. Ad esempio:

```PHP
$stringa = 'Questo è un esempio di stringa.';
$sottostringa = substr($stringa, 4, 10); // Estrae 10 caratteri a partire dal quinto carattere
echo $sottostringa; // Output: o è un es
```

## Approfondimento:
L'estrazione di sottostringhe è una tecnica comune utilizzata dai programmatori per manipolare le stringhe in vari modi. In passato, era necessario utilizzare funzioni come `strpos()` e `str_replace()` per manipolare le stringhe, ma con `substr()` possiamo ottenere più flessibilità e controllo sulla porzione di stringa da estrarre.

Un'alternativa alla funzione `substr()` è l'utilizzo del linguaggio di interrogazione delle espressioni regolari (regex). Con le regex, possiamo creare modelli per selezionare specifiche sottostringhe all'interno di una stringa. Tuttavia, l'utilizzo di regex può essere più complesso e richiedere più codice rispetto alla semplice funzione `substr()`.

L'implementazione della funzione `substr()` dipende dalla versione di PHP che si sta utilizzando. Nelle versioni precedenti a PHP 7, quest'ultima era sensibile al multibyte, il che significava che i caratteri multi-byte (come quelli utilizzati in alcune lingue come il cinese o il giapponese) venivano considerati come più di un singolo carattere. Tuttavia, a partire da PHP 7, questo problema è stato risolto e la funzione `mb_substr()` è stata introdotta per garantire una corretta gestione dei caratteri multi-byte.

## Vedi anche:
- La documentazione ufficiale di PHP sulla funzione `substr()`: https://www.php.net/manual/en/function.substr.php
- Un tutorial su come utilizzare le espressioni regolari (regex) per l'estrazione di sottostringhe: https://www.sitepoint.com/learn-regex-the-basics/