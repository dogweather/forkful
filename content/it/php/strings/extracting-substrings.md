---
title:                "Estrazione di sottostringhe"
aliases: - /it/php/extracting-substrings.md
date:                  2024-01-20T17:46:05.676902-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Estrarre sottostringhe significa prendere pezzetti di testo da una stringa più grande. Lo facciamo per analizzare, manipolare o validare dati e rendere dinamiche le nostre applicazioni PHP.

## How to: (Come fare:)
```PHP
<?php
$text = "Ciao, mondo della programmazione PHP!";
// Estrai "mondo" dalla stringa
$sub = substr($text, 6, 5);
echo $sub; // Output: mondo

// Ottieni l'ultima parte della stringa
$end = substr($text, -10);
echo $end; // Output: ogrammazione!
?>
```

## Deep Dive (Analisi Approfondita)
Historical context (Contesto Storico): La funzione `substr()` esiste da when PHP was just a personal tool for Rasmus Lerdorf, effectively serving developers since the earliest versions of PHP.

Alternatives (Alternative): Ci sono altre funzioni in PHP per la manipolazione delle stringhe, come `mb_substr()` per la compatibilità con multi-byte character encodings (utile per UTF-8) e `strstr()` per trovare tutto dopo una certa sottostringa.

Implementation details (Dettagli di Implementazione): `substr()` può avere un comportamento sorprendente con stringhe multibyte se non utilizzata correttamente, assicurati di utilizzare il set di caratteri giusto per evitare errori.

## See Also (Vedi Anche)
- La documentazione ufficiale di PHP su `substr()`: https://www.php.net/manual/en/function.substr.php
- Un confronto tra `substr()` e `mb_substr()`: https://www.php.net/manual/en/function.mb-substr.php
- La community di Stack Overflow dove chiedere aiuto: https://stackoverflow.com/questions/tagged/php
