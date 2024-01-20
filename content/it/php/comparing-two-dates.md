---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Confrontare due date è l'atto di determinare quale sia la più recente o se sono la stessa. I programmatori lo fanno per una serie di motivi, come determinare l'ordine degli eventi o calcolare il tempo trascorso tra due date.

## Come fare:

In PHP, è possibile utilizzare l'oggetto DateTime per confrontare due date. Ecco come:

```PHP
$date1 = new DateTime('2022-03-01');
$date2 = new DateTime('2022-04-01');

if ($date1 > $date2) {
    echo 'La data1 è più recente di data2';
} else if ($date1 < $date2) {
    echo 'La data2 è più recente di data1';
} else {
    echo 'Le date sono uguali';
}
```

L'output sarebbe:

```
La data2 è più recente di data1
```

## Approfondimento

Nel passato, prima dell'introduzione dell'oggetto DateTime in PHP 5.2.0, i programmatori dovevano convertire le date in timestamp UNIX usando la funzione `strtotime()`, per poi compararle.

Come alternativa, si possono sempre utilizzare le funzioni `strtotime()` o `date()`, ma l'uso della classe DateTime è più moderno e fornisce un'interfaccia più robusta.

Quando si confrontano le date con l'oggetto DateTime, è importante ricordare che la valutazione avviene confrontando i timestamp UNIX delle date. Pertanto, luci ombre e subtilità relative all'orario estivo e ai fusi orari vengono automaticamente gestiti dalla classe DateTime.

## Vedere Anche

Per saperne di più sulla confrontazione delle date in PHP:

1. Documentazione ufficiale PHP per la classe DateTime:https://www.php.net/manual/en/class.datetime.php
2. Guida utile su come confrontare le date in PHP: https://stackoverflow.com/questions/16825240/how-to-compare-two-dates-in-php
3. Per differenza tra due date, l'uso di DateInterval: https://www.php.net/manual/en/datetime.diff.php