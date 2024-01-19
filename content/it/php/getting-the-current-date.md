---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?

Ottenere la data corrente è un compito comune in PHP e si riferisce all'azione di determinare la data e l'orario precisi del momento in cui viene eseguito il codice. Gli sviluppatori lo fanno per tracciare eventi, emettere timestamp e gestire funzionalità di pianificazione.

## Come fare:

In PHP otteniamo la data corrente usando la funzione `date()`. Ecco un esempio:

```PHP
<?php
echo date('d-m-Y');
?>
```

Questo restituirà la data corrente nel formato 'gg-mm-aaaa'. 

## Approfondimenti:

Storicamente, PHP ha avuto un supporto limitato per la gestione del tempo e delle date, ma dal PHP 5.2.0 è stato introdotto un approccio molto più robusto grazie alla classe `DateTime`. Questa classe fornisce metodi per manipolare date e tempi in vari formati. Ecco un esempio:

```PHP
<?php
$oggi = new DateTime();
echo $oggi->format('d-m-Y');
?>
```

In aggiunta alla funzione `date()`, PHP offre anche la funzione `time()`, che restituisce il timestamp Unix corrente, un grande numero che rappresenta i secondi trascorsi dal 1° gennaio 1970. Questa funzione può essere utile se devi calcolare la differenza tra due date.

```PHP
<?php
$timestamp = time();
echo $timestamp;
?>
```

## Vedi anche:

Per una comprensione più profonda delle funzioni di data e ora in PHP, controlla i seguenti link:

1. [PHP: date - Manual](https://www.php.net/manual/en/function.date.php)
2. [PHP: DateTime - Manual](https://www.php.net/manual/en/class.datetime.php)
3. [PHP: time - Manual](https://www.php.net/manual/en/function.time.php)