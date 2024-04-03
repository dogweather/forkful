---
date: 2024-01-20 17:42:50.479350-07:00
description: 'How to: "Codice:" Usa le funzioni preg_replace e str_replace per eliminare
  i caratteri. Ecco degli esempi.'
lastmod: '2024-03-13T22:44:43.501692-06:00'
model: gpt-4-1106-preview
summary: '"Codice:"

  Usa le funzioni preg_replace e str_replace per eliminare i caratteri.'
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## How to:
"Codice:"
Usa le funzioni preg_replace e str_replace per eliminare i caratteri. Ecco degli esempi:

```PHP
<?php
// Utilizzando preg_replace con espressioni regolari
$string = "Ciao M0nd0, PHP è f4nt4st1c0!";
$pattern = '/[0-9]/'; // Cerchiamo di eliminare tutti i numeri
$replaced = preg_replace($pattern, '', $string);
echo $replaced;  // Output: Ciao Mndo, PHP è fntstc!

// Utilizzando str_replace per eliminare caratteri specifici
$string2 = "Hello, World!";
$remove = ["l", "d"];
$replaced2 = str_replace($remove, '', $string2);
echo $replaced2;  // Output: Heo, Wor!
?>
```

## Deep Dive:
"Approfondimento:"
La possibilità di eliminare caratteri con criteri specifici è stata una necessità da quando si è iniziato a lavorare con testi programmazione. Preg_replace usa un potente strumento chiamato espressioni regolari, nato negli anni '50 e perfetto per questi compiti. Str_replace è più semplice e veloce ma meno flessibile. In PHP, preg_replace è consigliato per pattern complessi, mentre str_replace per sostituzioni semplici. L'implementazione di questi strumenti sfrutta algoritmi di ricerca e sostituzione ottimizzati per efficienza.

## See Also:
"Vedi Anche:"
Per approfondire, ecco dove andare:

- PHP Official Documentation for preg_replace: [PHP: preg_replace - Manual](https://www.php.net/manual/en/function.preg-replace.php)
- PHP Official Documentation for str_replace: [PHP: str_replace - Manual](https://www.php.net/manual/en/function.str-replace.php)
- Tutorial sulle espressioni regolari: [Regular-Expressions.info](https://www.regular-expressions.info)
