---
title:                "Ottenere la data corrente"
date:                  2024-02-03T19:10:08.655958-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Ottenere la data corrente in PHP è un'attività fondamentale che permette di recuperare e manipolare la data e l'ora del sistema. Questo è cruciale per funzioni come il logging, l'impostazione di timestamp nei post, la programmazione di eventi o l'esecuzione di operazioni sensibili al tempo nelle tue applicazioni.

## Come fare:
### PHP Nativo
La funzione integrata `date()` di PHP è il modo più diretto per ottenere la data corrente. Puoi formattare la data in vari modi specificando il parametro di formato.

```php
echo date("Y-m-d"); // Output: 2023-04-01 (per esempio)
echo date("l, F j, Y"); // Output: Sabato, Aprile 1, 2023
```

Per ottenere la data e l'ora con supporto al fuso orario, puoi usare la classe `DateTime` insieme a `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Output: 2023-04-01 12:00:00 (per esempio)
```

### Utilizzando Carbon (Una Popolare Libreria di Terze Parti)
[Carbon](https://carbon.nesbot.com/) è un'estensione API semplice per `DateTime` che fornisce un modo più pulito e fluente per lavorare con date e orari.

Prima, assicurati di avere Carbon installato tramite Composer:
```bash
composer require nesbot/carbon
```

Poi, puoi usarlo per ottenere la data corrente:

```php
use Carbon\Carbon;

echo Carbon::now(); // Output: 2023-04-01 12:00:00 (per esempio, nel formato predefinito)
echo Carbon::now()->toDateString(); // Output: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Output: Sabato, Aprile 1, 2023
```

Carbon arricchisce la gestione delle date in PHP aggiungendo leggibilità e una ricchezza di funzionalità per la manipolazione, il confronto e la formattazione del tempo.
