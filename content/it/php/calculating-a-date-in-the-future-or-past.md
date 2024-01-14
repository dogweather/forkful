---
title:    "PHP: Calcolare una data nel futuro o nel passato."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

Se stai lavorando su un progetto di programmazione che richiede la gestione di date e orari, potresti trovarmi nella situazione in cui hai bisogno di calcolare una certa data in futuro o passato. In questi casi, avere una conoscenza dei metodi e delle funzioni di PHP per la gestione delle date può essere molto utile e semplificare il tuo lavoro.

## Come fare

Per calcolare una data in futuro o passato in PHP, ci sono alcune funzioni e metodi che possono essere utilizzati. Uno dei più utili è `strtotime()`, che convertirà una stringa di data in un timestamp Unix. Ad esempio:

```PHP 
$data = '3 March 2021';
$timestamp = strtotime($data);
echo $timestamp; // Outputs 1614758400
```

Per calcolare una data in futuro o passato, puoi quindi utilizzare la funzione `date()` per formattare il timestamp in una data in un formato specifico. Ecco un esempio di come calcolare la data di un mese fa:

```PHP
$timestamp = strtotime('-1 month');
$data = date('d/m/Y', $timestamp);
echo $data; // Outputs 26/02/2021
```

È possibile utilizzare diverse parole chiave come "year" (anno), "day" (giorno) o "hour" (ora), che possono essere utilizzate per calcolare una data in futuro o passato in base a tali unità di tempo.

## Approfondimento

Se vuoi approfondire la gestione delle date in PHP, ci sono alcune funzioni e metodi che possono essere utili da conoscere. Ad esempio, la funzione `date_parse()` può essere utilizzata per analizzare una stringa di data e restituisce un array con le informazioni relative alla data. Inoltre, la libreria `DateTime` offre una vasta gamma di metodi per la manipolazione delle date e degli orari.

Inoltre, è possibile gestire date e orari secondo diversi fusi orari utilizzando la funzione `date_default_timezone_set()`. Ci sono anche molte librerie esterne che possono semplificare ulteriormente la gestione delle date in PHP, come ad esempio Carbon e DateTimeImmutable.

## Vedi anche

- [Documentazione sulle funzioni di gestione delle date in PHP](https://www.php.net/manual/en/book.datetime.php)
- [Libreria Carbon per la gestione delle date in PHP](https://carbon.nesbot.com/)
- [Libreria DateTimeImmutable per la gestione delle date in PHP](https://www.php.net/manual/en/class.datetimeimmutable.php)