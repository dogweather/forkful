---
title:                "Calcolo di una data nel futuro o nel passato"
html_title:           "PHP: Calcolo di una data nel futuro o nel passato"
simple_title:         "Calcolo di una data nel futuro o nel passato"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Calcolare una data in futuro o in passato è un'operazione comune per i programmatori. Essenzialmente, si tratta di aggiungere o sottrarre un numero di giorni, settimane o mesi da una data specifica. Questo può essere utile per automatizzare la generazione di scadenze o per la gestione di eventi in calendario.

## Come fare:
Ecco alcuni esempi di codice PHP per calcolare una data in futuro o in passato:

```PHP
// Aggiungere 15 giorni ad oggi
$date = date('Y-m-d', strtotime('+15 days'));
echo $date; // Output: 2020-12-09

// Sottrarre 2 mesi da una data specifica
$date = date('Y-m-d', strtotime('2020-10-15 -2 months'));
echo $date; // Output: 2020-08-15

// Aggiungere 1 settimana a una data specifica
$date = date('Y-m-d', strtotime('2020-11-25 +1 week'));
echo $date; // Output: 2020-12-02
```

## Approfondimento:
Ci sono diverse alternative per calcolare una data in futuro o in passato, come l'utilizzo della funzione `mktime()` o l'utilizzo di librerie esterne come Carbon o DateTime. Inoltre, è importante tenere conto di fattori come i giorni festivi o il cambio dell'ora legale quando si effettuano questi calcoli.

## Vedi anche:
- [Funzione date() di PHP](https://www.php.net/manual/en/function.date.php)
- [Funzione strtotime() di PHP](https://www.php.net/manual/en/function.strtotime.php)
- [Libreria Carbon per PHP](https://carbon.nesbot.com/)
- [Classe DateTime di PHP](https://www.php.net/manual/en/class.datetime.php)