---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
L'analisi di una data da una stringa consiste nello scomporre un testo che rappresenta una data in parti più piccole e significative. I programmatori la usano per interpretare e manipolare le date in modo più comodo e funzionale nelle loro applicazioni.

## Come fare:
Utilizzare la funzione PHP `date_parse` o `DateTime::createFromFormat` per analizzare una data. Ecco alcuni esempi:

```PHP
<?php
$dataStringa = "2021-11-30";
$data = date_parse($dataStringa);

var_dump($data);
?>
```

Questo produrrà un output come questo:

```PHP
array(13) {
  ["year"]=>
  int(2021)
  ["month"]=>
  int(11)
  ["day"]=>
  int(30)
  ...
}
```

O utilizzare "DateTime::createFromFormat":

```PHP
<?php
$dataStringa = "Novembre 30, 2021";
$data = DateTime::createFromFormat('F j, Y', $dataStringa);

echo $data->format('Y-m-d');
?>
```

Questo restituirà:

```PHP
2021-11-30
```

## Approfondimento
L'analisi della data da una stringa è una pratica comune da quando le date sono state prima rappresentate come stringhe di testo, piuttosto che come numeri o altri tipi. Alcune alternative all'analisi standard di PHP includono l'uso di librerie esterne, come Carbon per PHP. Queste librerie possono fornire maggiori funzionalità e flessibilità, ma richiedono una dipendenza esterna. Dettagli sull'implementazione dell'analisi delle date in PHP sono disponibili nel manuale PHP e nei commenti degli utenti.

## Vedi Anche
Per ulteriori dettagli e opzioni, consultare la documentazione PHP ufficiale e le guide disponibili online.

- [Funzioni di data e ora di PHP](https://www.php.net/manual/it/book.datetime.php)
- [Parser di sintassi datetime compatibile con 
Strtotime()](https://www.php.net/manual/it/datetime.formats.date.php)
- [Carbon - Una semplice libreria PHP per Date and Time](https://carbon.nesbot.com/)