---
title:                "Calcolare una data nel futuro o nel passato."
html_title:           "PHP: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Capita spesso di dover calcolare una data nel futuro o nel passato per scopi pratici o di programmazione. Conoscere come farlo in modo efficiente può semplificare il lavoro e risparmiare tempo.

## Come Fare

Per calcolare una data nel futuro o nel passato con PHP possiamo utilizzare la funzione `strtotime()`. Questa funzione accetta come primo argomento una stringa contenente la data da modificare e come secondo argomento la data di riferimento rispetto alla quale effettuare il calcolo. Possiamo anche specificare un terzo argomento opzionale per formattare la data di output.

Esempio:

```
<?php
// Calcola la data 1 settimana nel futuro rispetto alla data odierna
$date = date('Y-m-d', strtotime('+1 week'));

echo $date;
// Output: 2021-08-25
?>
```

In questo esempio abbiamo utilizzato `strtotime()` con `+1 week` come primo argomento per calcolare una data 1 settimana nel futuro rispetto alla data odierna. Se invece vogliamo calcolare una data nel passato possiamo utilizzare un valore negativo come `-1 week`.

## Deep Dive

La funzione `strtotime()` ci permette di utilizzare anche parole chiave come `tomorrow`, `next week`, `last monday`, ecc. per calcolare una data. Inoltre, possiamo combinare più parole chiave per ottenere date ancora più precise.

Esempio:

```
<?php
// Calcola la data di domani
$date1 = date('Y-m-d', strtotime('tomorrow'));

echo $date1;
// Output: 2021-08-19

// Calcola la data del prossimo martedì
$date2 = date('Y-m-d', strtotime('next tuesday'));

echo $date2;
// Output: 2021-08-24
?>
```

Inoltre, è possibile utilizzare la funzione `date()` per modificare il formato di output della data calcolata.

Esempio:

```
<?php
// Calcola la data del prossimo mese
$date = date('Y-m-d', strtotime('next month'));

echo date('d-m-Y', strtotime($date));
// Output: 01-09-2021
?>
```

È importante tenere presente che la funzione `strtotime()` lavora in base all'orario corrente impostato sul server. Se si vuole utilizzare una data specifica come riferimento, è meglio utilizzare la funzione `mktime()`.

## Vedi Anche

- [Documentazione ufficiale di PHP su strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [Guida su come utilizzare le funzioni di data e ora in PHP](https://www.php.net/manual/en/book.datetime.php)