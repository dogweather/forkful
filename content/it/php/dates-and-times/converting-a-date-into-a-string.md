---
date: 2024-01-20 17:37:06.345250-07:00
description: "How to: Convertire le date in stringhe \xE8 un'operazione effettuata\
  \ gi\xE0 dai tempi del PHP 4, sebbene con meno funzionalit\xE0. Con l'introduzione\
  \ della classe\u2026"
lastmod: '2024-04-05T21:53:44.292400-06:00'
model: gpt-4-1106-preview
summary: "Convertire le date in stringhe \xE8 un'operazione effettuata gi\xE0 dai\
  \ tempi del PHP 4, sebbene con meno funzionalit\xE0."
title: Conversione di una data in una stringa
weight: 28
---

## How to:
```PHP
<?php
$data = new DateTime('now', new DateTimeZone('Europe/Rome'));
$formatoItaliano = $data->format('d-m-Y H:i:s');
echo $formatoItaliano;  // Output esempio: 23-03-2023 15:42:01
?>
```

```PHP
// Un altro esempio con le date internazionali (ISO 8601)
echo date('c');  // Output esempio: 2023-03-23T15:42:01+01:00
?>
```

## Deep Dive
Convertire le date in stringhe è un'operazione effettuata già dai tempi del PHP 4, sebbene con meno funzionalità. Con l'introduzione della classe DateTime in PHP 5, la gestione delle date è diventata più potente e flessibile. Esistono molteplici formati per la conversione: oltre a quelli italiani, si può usare lo standard ISO 8601 o formati personalizzati. Attenzione a gestire correttamente il fuso orario con `DateTimeZone` per evitare incongruenze.

## See Also
- La documentazione ufficiale PHP sulla classe DateTime: [PHP: DateTime - Manual](https://www.php.net/manual/en/class.datetime.php)
- Funzioni di formattazione delle date di PHP: [PHP: date - Manual](https://www.php.net/manual/en/function.date.php)
- Informazioni sui fusi orari disponibili in PHP: [PHP: List of Supported Timezones - Manual](https://www.php.net/manual/en/timezones.php)
