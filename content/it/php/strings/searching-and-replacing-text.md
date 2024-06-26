---
date: 2024-01-20 17:58:16.123144-07:00
description: 'How to (Come fare): Ecco un esempio semplice in PHP.'
lastmod: '2024-03-13T22:44:43.502615-06:00'
model: gpt-4-1106-preview
summary: Ecco un esempio semplice in PHP.
title: Ricerca e sostituzione del testo
weight: 10
---

## How to (Come fare):
Ecco un esempio semplice in PHP:

```PHP
<?php
$originale = "Ciao mondo!";
$sostituire = str_replace("mondo", "universo", $originale);
echo $sostituire;  // Stampa "Ciao universo!"
?>
```
E per sostituzioni basate su pattern complessi, usiamo le espressioni regolari:

```PHP
<?php
$testo = "PHP è fantastico!";
$pattern = "/fantastico/";
$sostituzione = "incredibile";
$novo_testo = preg_replace($pattern, $sostituzione, $testo);
echo $novo_testo;  // Stampa "PHP è incredibile!"
?>
```

## Deep Dive (Approfondimenti):
La sostituzione di testo esiste da quando è iniziata la programmazione. Funzioni come `str_replace()` e `str_ireplace()` (che ignora la differenza tra maiuscole e minuscole) sono essenziali in PHP. 

Le espressioni regolari, introdotte negli anni '50, hanno conquistato PHP con `preg_replace()`, basato sul motore PCRE (Perl Compatible Regular Expressions). Più potenti delle semplici funzioni di sostituzione, permettono pattern complessi e flessibili. Per esempio, per sostituire tutti i colori in una frase con la parola "colore":

```PHP
$frase = "Le rose sono rosse, il cielo è azzurro.";
$frase_modificata = preg_replace("/rosso|azzurro/", "colore", $frase);
echo $frase_modificata;  // Stampa "Le rose sono colore, il cielo è colore."
```
Questo mostra come un singolo pattern possa corrispondere a più casi.

## See Also (Vedi Anche):
- [PHP str_replace() Function](https://www.php.net/manual/en/function.str-replace.php)
- [PHP preg_replace() Function](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions (Regex) - A Simple, non-technical explanation](https://www.regular-expressions.info/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)

Questi link portano a documentazione ufficiale e risorse per approfondire l'uso delle stringhe e delle espressioni regolari in PHP.
