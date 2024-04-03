---
date: 2024-01-20 17:47:57.068265-07:00
description: "Misurare la lunghezza di una stringa significa contare i caratteri che\
  \ la compongono. I programmatori lo fanno per validare l\u2019input, troncare il\
  \ testo o\u2026"
lastmod: '2024-03-13T22:44:43.508005-06:00'
model: gpt-4-1106-preview
summary: Misurare la lunghezza di una stringa significa contare i caratteri che la
  compongono.
title: Trovare la lunghezza di una stringa
weight: 7
---

## What & Why?
Misurare la lunghezza di una stringa significa contare i caratteri che la compongono. I programmatori lo fanno per validare l’input, troncare il testo o semplicemente per analisi.

## How to:
PHP ti offre `strlen()` per contare i caratteri. Vediamo come:

```PHP
<?php
$testo = "Ciao, mondo!";
echo strlen($testo); // Risultato: 13
?>
```
Se hai bisogno di contare i caratteri multibyte (come quelli UTF-8), usa `mb_strlen()`:

```PHP
<?php
$testo = "Caffè";
echo mb_strlen($testo, "UTF-8"); // Risultato: 5
?>
```

## Deep Dive
Fin dagli albori, PHP ha reso la gestione delle stringhe un gioco da ragazzi. Ma attenzione: `strlen()` conta i byte, non i caratteri. Quindi funziona bene con il set di caratteri a singolo byte come ASCII. 

Con l'introduzione di UTF-8 e altri set di caratteri multibyte, `mb_strlen()` è diventato essenziale. Infatti, considera correttamente i caratteri composti da più byte.

Altre funzioni utili sono `substr()` per estrarre sottostringhe e `strpos()` per trovare la posizione di una sottostringa.

Ah, una curiosità: PHP 8 ha fatto miglioramenti nel trattamento delle stringhe multibyte, rendendo la gestione ancora più solida e affidabile.

## See Also
- [Documentazione ufficiale di strlen()](https://www.php.net/manual/en/function.strlen.php)
- [Documentazione ufficiale di mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [Tutorial PHP sulle stringhe](https://www.php.net/manual/en/language.types.string.php)
