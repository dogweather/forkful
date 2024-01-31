---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:37:49.507862-07:00
simple_title:         "Estrarre una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(Parse date da stringa: Che cos'è e perché?)
Stiamo parlando di trasformare una data in formato testuale in una struttura di dati che PHP può capire e manipolare. I programmatori fanno questo per leggere date da fonti come file di testo, database e input utente, e per usarle in operazioni come comparazioni, calcoli e formattazioni.

## How to:
(Come fare:)
PHP fornisce funzioni potenti per gestire date e orari. Ecco un esempio semplice usando `DateTime::createFromFormat`:

```PHP
<?php
$dataTestuale = "25-12-2023";

// Crea un oggetto DateTime da una stringa data secondo un formato specificato
$dataOggetto = DateTime::createFromFormat('d-m-Y', $dataTestuale);

// Stampa il risultato
echo $dataOggetto->format('Y-m-d'); // Output: "2023-12-25"
?>
```

Semplice, vero? Sul serio, non c'è molto di più per i casi di uso quotidiano.

## Deep Dive:
(Affondiamo un poco più in profondità:)
Prima di PHP 5.2.0, la gestione delle date era un po' più artigianale. Dovevamo affidarci a `strtotime()` e alle sue limitazioni. Oggi abbiamo `DateTime`, che non solo legge date da stringhe, ma gestisce timezone e DST (Daylight Saving Time, ovvero l'ora legale). E se la data è in un formato strano? Nessun problema, `createFromFormat()` accetta quasi qualunque cosa gli lanci, purché tu gli dia l'alfabeto per decifrarlo.

E se `DateTime` è troppo per te? Esistono alternative come `Carbon`, una libreria estesa che rende alcune operazioni ancora più semplici. Ma ricorda, più potere significa più responsabilità. Usare formato e timezone sbagliati può portare a date e orari errati, quindi attenzione.

## See Also:
(Fonti correlate:)
- [Documentazione ufficiale di PHP su DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Documentazione su DateTimeImmutable](https://www.php.net/manual/en/class.datetimeimmutable.php)
- [Carbon: Una semplice libreria di date per PHP](https://carbon.nesbot.com/docs/)

Questi link sono un ottimo punto di partenza per approfondire. Buona codifica!
