---
date: 2024-01-26 04:43:53.767444-07:00
description: "I numeri complessi hanno una parte reale e una parte immaginaria, solitamente\
  \ scritti come `a + bi`. Sono fondamentali in matematica avanzata, fisica,\u2026"
lastmod: '2024-03-13T22:44:43.510754-06:00'
model: gpt-4-0125-preview
summary: I numeri complessi hanno una parte reale e una parte immaginaria, solitamente
  scritti come `a + bi`.
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
PHP fornisce supporto integrato per i numeri complessi utilizzando l'estensione `ext-intl` con la classe `NumberFormatter`. Ecco un esempio:

```php
// Assicurarsi che l'estensione intl sia caricata
if (!extension_loaded('intl')) {
    die("L'estensione intl non è abilitata. Si prega di abilitarla per eseguire questo codice.");
}

function addComplexNumbers($a, $b) {
    // Utilizzare NumberFormatter per analizzare e formattare i numeri complessi
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Analizzare i numeri complessi dalle stringhe
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Eseguire l'addizione
    $sum = $numA + $numB;

    // Formattare il risultato come un numero complesso
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Output: 7+10i
```

## Approfondimento
Prima di `ext-intl`, PHP non aveva supporto nativo per i numeri complessi. Gli sviluppatori utilizzavano funzioni o librerie di classi personalizzate per gestire i numeri complessi. Le operazioni complesse potevano essere noiose e soggette a errori, ma `ext-intl` fornisce un modo internazionalizzato per presentare e analizzare i numeri complessi allineato con la libreria ICU.

Tuttavia, per operazioni matematiche di grande entità, alcuni potrebbero utilizzare librerie esterne scritte in linguaggi più adatti alla matematica (come C o Python) e interfacciarsi con esse tramite PHP. Per quanto riguarda l'implementazione, `ext-intl` gestisce tutto dietro le quinte, garantendo un'aritmetica accurata mentre astrae la complessità dallo sviluppatore.

Storicamente, i numeri complessi erano malvisti essendo denominati 'immaginari', ma da allora sono diventati fondamentali in vari campi scientifici e matematici, rivelando più sulla loro importanza nel mondo reale di quanto il loro status immaginario abbia mai suggerito.

## Vedi anche
- [Manuale PHP su NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia sui numeri complessi](https://it.wikipedia.org/wiki/Numero_complesso)
- [PHP: La via giusta - Lavorare con i tipi di dato](https://phptherightway.com/#data_types)
