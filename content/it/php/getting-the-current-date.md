---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:15:53.865681-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in PHP è come dare un'occhiata all'orologio. Si fa perché spesso si vuole registrare un evento, comparare le date, o semplicemente visualizzarla.

## How to:
Ecco il modo più diretto per ottenere la data corrente:

```PHP
<?php
echo date('Y-m-d'); // output: YYYY-MM-DD
?>
```

Vuoi anche l'ora? Nessun problema:

```PHP
<?php
echo date('Y-m-d H:i:s'); // output: YYYY-MM-DD HH:MM:SS
?>
```

O magari vuoi l'ora in formato italiano? Facile:

```PHP
<?php
setlocale(LC_TIME, 'it_IT');
echo strftime('%e %B %Y'); // output: 1 gennaio 2023
?>
```

## Deep Dive
L'uso della funzione `date()` è un classico in PHP. Nata insieme al linguaggio negli anni '90, è rimasta il modo più semplice per gestire date e orari. Alternative moderne includono l'oggetto `DateTime`, che offre maggior flessibilità e funzioni aggiuntive:

```PHP
<?php
$oggi = new DateTime();
echo $oggi->format('Y-m-d H:i:s'); // stessa output di date()
?>
```

La funzione `strftime()`, invece, è stata deprecata in PHP 8.1. Meglio usare `DateTime` per formati localizzati:

```PHP
<?php
$formatter = new IntlDateFormatter('it_IT', IntlDateFormatter::LONG, IntlDateFormatter::NONE);
echo $formatter->format(new DateTime()); // output simile a strftime()
?>
```

La gestione di date e orari si basa sulle impostazioni di configurazione del server PHP (php.ini) e sulla libreria ICU per la localizzazione.

## See Also
- La documentazione ufficiale di PHP sulle date: [https://www.php.net/manual/en/book.datetime.php](https://www.php.net/manual/en/book.datetime.php)
- Informazioni sull'oggetto DateTime: [https://www.php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Guida alle configurazioni php.ini: [https://www.php.net/manual/en/ini.core.php#ini.date.timezone](https://www.php.net/manual/en/ini.core.php#ini.date.timezone)
