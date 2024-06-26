---
date: 2024-01-20 17:31:27.744524-07:00
description: "Come fare: Le funzioni per la manipolazione delle date in PHP sono evolute\
  \ nel tempo. In PHP 5.2.0, \xE8 stata introdotta la classe `DateTime`, che fornisce\u2026"
lastmod: '2024-04-05T21:53:44.294136-06:00'
model: gpt-4-1106-preview
summary: Le funzioni per la manipolazione delle date in PHP sono evolute nel tempo.
title: Calcolo di una data futura o passata
weight: 26
---

## Come fare:
```PHP
<?php
// Data attuale
$oggi = new DateTime();

// Calcolo di una data nel futuro (+2 settimane)
$due_settimane_dopo = clone $oggi;
$due_settimane_dopo->modify('+2 weeks');
echo "Due settimane dopo: " . $due_settimane_dopo->format('Y-m-d') . "\n";

// Calcolo di una data nel passato (-1 mese)
$un_mese_prima = clone $oggi;
$un_mese_prima->modify('-1 month');
echo "Un mese prima: " . $un_mese_prima->format('Y-m-d') . "\n";
?>
```
Output:
```
Due settimane dopo: 2023-04-14
Un mese prima: 2023-03-07
```

## Approfondimento
Le funzioni per la manipolazione delle date in PHP sono evolute nel tempo. In PHP 5.2.0, è stata introdotta la classe `DateTime`, che fornisce un modo più completo e orientato agli oggetti per lavorare con date e ore rispetto alle tradizionali funzioni come `strtotime()` e `date()`. Con `DateTime` è più semplice aggiungere o sottrarre intervalli di tempo con il metodo `modify()` o usando `DateInterval`.

Esistono alternative, come l'uso delle funzioni `strtotime()` e `date()`, ma queste possono diventare verbose e meno intuitive rispetto alla manipolazione attraverso `DateTime`.

Dettaglio implementazione: quando si utilizzano intervalli di tempo, prestare attenzione ai cambi di ora legale. `DateTime` gestisce automaticamente questi cambi, ma quando si usano funzioni come `strtotime()`, è necessario essere cauti.

## Leggi anche
- La documentazione di PHP su DateTime: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Funzione `strtotime()`: [php.net/manual/en/function.strtotime.php](https://www.php.net/manual/en/function.strtotime.php)
- La guida di PHP ai formati di data e ora: [php.net/manual/en/datetime.formats.php](https://www.php.net/manual/en/datetime.formats.php)
