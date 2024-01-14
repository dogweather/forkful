---
title:                "PHP: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
In questo post, parleremo di come ottenere la data corrente utilizzando PHP. Ottenere la data corrente può essere molto utile in diverse situazioni, come per esempio per ottenere il timestamp per creare nomi di file unici o per mostrare la data corretta nei tuoi progetti.

## Come Fare
Per ottenere la data corrente in PHP, possiamo utilizzare la funzione `date()` passando come parametro il formato di data desiderato. Ad esempio, se vogliamo ottenere la data nel formato gg-mm-aaaa, possiamo utilizzare il seguente codice:
```PHP
<?php
$date = date('d-m-Y');
echo $date;
```
Questo codice stamperebbe la data corrente nel formato desiderato, ad esempio "06-05-2021". Possiamo anche specificare il formato del tempo, con o senza AM/PM. Ecco alcuni esempi:
```PHP
<?php
// Data e ora con AM/PM
$datetime = date('d-m-Y h:i:s A');
echo $datetime; // Stampa: 06-05-2021 05:10:03 PM

// Solo tempo
$time = date('h:i:s');
echo $time; // Stampa: 05:10:03
```

## Deep Dive
Ora che abbiamo visto come ottenere la data corrente, vediamo quali altri parametri possiamo passare alla funzione `date()` per personalizzare il formato della data. Alcuni dei parametri più comuni sono:

- `d`: giorno del mese (01-31)
- `m`: mese (01-12)
- `Y`: anno con 4 cifre (es. 2021)
- `y`: anno con 2 cifre (es. 21)
- `h`: ora (01-12)
- `H`: ora (00-23)
- `i`: minuti (00-59)
- `s`: secondi (00-59)
- `A`: AM/PM (AM o PM)
- `l`: giorno della settimana (Monday, Tuesday, etc.)
- `z`: giorno dell'anno (0-365)
- `M`: mese abbreviato (Jan, Feb, etc.)
- `F`: mese completo (January, February, etc.)

Per una lista completa dei parametri disponibili, puoi consultare la documentazione ufficiale di PHP.

## Vedi Anche
- [Funzione `date()` su PHP.net](https://www.php.net/manual/en/function.date.php)
- [Tutorial PHP: Date and Time](https://www.w3schools.com/php/php_date.asp)
- [Come ottenere la data corrente in PHP](https://www.geeksforgeeks.org/how-to-get-current-date-and-time-in-php/)