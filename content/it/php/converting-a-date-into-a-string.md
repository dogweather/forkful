---
title:                "Convertire una data in una stringa"
html_title:           "PHP: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è un'operazione comune quando si lavora con le date in PHP. Ciò può essere utile per visualizzare le date in un formato specifico o per manipolare i dati in modo più semplice.

## Come Fare

Per convertire una data in una stringa in PHP, possiamo utilizzare la funzione `date()` che accetta due parametri: il formato della data desiderato e la data stessa.

```PHP
$date = date('d/m/Y', time()); // Restituisce la data odierna nel formato "giorno/mese/anno"
echo $date; // Output: 18/07/2020
```

Possiamo anche specificare una data diversa da quella odierna utilizzando la funzione `strtotime()`.

```PHP
$date = date('F j, Y', strtotime('2020-12-25')); // Restituisce la data del 25 dicembre 2020 nel formato "mese giorno, anno"
echo $date; // Output: December 25, 2020
```

Inoltre, possiamo utilizzare le lettere chiave del formato per personalizzare ulteriormente la nostra stringa di data. Ad esempio:

```PHP
$date = date('d \d\e\l m \d\e\l Y', strtotime('yesterday')); // Restituisce la data di ieri nel formato "dd del mm del yyyy"
echo $date; // Output: 17 del 07 del 2020
```

Ci sono molte altre possibili combinazioni di formato e opzioni disponibili per la funzione `date()`. Assicurati di consultare la documentazione ufficiale di PHP per ulteriori dettagli.

## Approfondimento

La funzione `date()` utilizza il timestamp Unix come valore di default. Questo rappresenta il numero di secondi trascorsi dal 1° gennaio 1970 a mezzanotte (00:00:00) di una data specifica. Questo rende più facile per PHP calcolare date e orari, in quanto tutti i dati sono basati su un unico numero.

Inoltre, è possibile utilizzare la funzione `setlocale()` per specificare la lingua e il paese desiderati per la formattazione della data. Ad esempio, se si vuole visualizzare la data in italiano, possiamo utilizzare il seguente codice:

```PHP
setlocale(LC_TIME, 'it_IT');

$date = strftime('%A, %d %B %Y', time()); // Restituisce la data odierna nel formato "giorno della settimana, giorno mese anno"
echo $date; // Output: Sabato, 18 Luglio 2020
```

## Vedi Anche

- Documentazione ufficiale di PHP sulla funzione `date()`: https://www.php.net/manual/it/function.date.php
- Documentazione ufficiale di PHP sulla funzione `setlocale()`: https://www.php.net/manual/it/function.setlocale.php