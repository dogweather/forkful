---
title:                "PHP: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è spesso necessario in programmazione per poter controllare la validità di informazioni temporali. Ad esempio, si potrebbe voler verificare se una prenotazione è stata effettuata in una data precedente alla data odierna o se un evento è già avvenuto.

## Come fare

Per comparare due date in PHP, è possibile utilizzare la funzione "date_diff" che calcola la differenza tra due oggetti di tipo DateTime. Di seguito un esempio di codice che confronta due date e stampa il numero di giorni di differenza.

```PHP
$primaData = new DateTime('2021-01-01');
$secondaData = new DateTime('2021-01-10');
$differenza = date_diff($primaData, $secondaData);
echo $differenza->days;
```

L'output di questo esempio sarà "9", in quanto ci sono 9 giorni di differenza tra le due date. È inoltre possibile utilizzare altri metodi forniti dalla classe DateTime per effettuare confronti più complessi, come ad esempio confrontare l'ora o il fuso orario delle date.

## Approfondimento

Per una comparazione più approfondita tra due date, è importante conoscere alcune caratteristiche del formato di data e ora utilizzato. Ad esempio, se si utilizza il formato "Y-m-d" (anno-mese-giorno) per le date, non è possibile confrontare le ore o i minuti delle date. Inoltre, è importante assicurarsi che le date siano nello stesso fuso orario per evitare inconsistenze nei confronti.

## Vedi anche

- PHP Date and Time Functions: https://www.php.net/manual/en/ref.datetime.php
- Comparison of PHP DateTime objects: https://www.php.net/manual/en/datetime.diff.php