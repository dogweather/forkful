---
title:                "Confronto tra due date"
html_title:           "PHP: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore PHP e stai lavorando con le date, potresti trovarti nella situazione in cui devi confrontare due date per determinare quale è più recente o se sono uguali. Questo potrebbe essere utile, ad esempio, per gestire gli ordini in un negozio online o per gestire eventi futuri in un calendario.

## Come

Per confrontare due date in PHP, puoi utilizzare la funzione `strtotime()` per convertirle in timestamp e poi confrontare i timestamp con gli operatori `<`, `>` o `==` a seconda di quello che vuoi ottenere. Ad esempio:

```PHP
$date1 = "2020-05-21";
$date2 = "2019-12-31";

if (strtotime($date1) > strtotime($date2)) {
    echo "La data 1 è più recente della data 2";
} elseif (strtotime($date1) < strtotime($date2)) {
    echo "La data 2 è più recente della data 1";
} else {
    echo "Le date sono uguali";
}
```

In questo esempio, stiamo utilizzando la funzione `strtotime()` per convertire le due date in timestamp, che rappresentano il numero di secondi trascorsi dal 1° gennaio 1970. Successivamente, utilizzando gli operatori `<`, `>` e `==`, confrontiamo i timestamp e stampiamo il messaggio appropriato.

Puoi anche utilizzare la funzione `DateTime()` per creare oggetti data e confrontarli con il metodo `diff()`, che restituirà un oggetto `DateInterval` contenente l'intervallo tra le due date. Ad esempio:

```PHP
$date1 = new DateTime("2020-05-21");
$date2 = new DateTime("2019-12-31");

$interval = $date1->diff($date2);
if ($interval->invert) {
    echo "La data 2 è più recente della data 1";
} elseif (!$interval->days) {
    echo "Le date sono uguali";
} else {
    echo "La data 1 è più recente della data 2";
}
```

In questo esempio, stiamo creando due oggetti `DateTime` con le due date e poi utilizzando il metodo `diff()` per ottenere l'intervallo tra di esse. Il metodo `diff()` restituisce un oggetto `DateInterval` che contiene proprietà come `$interval->invert` (che indica se l'intervallo è negativo) e `$interval->days` (che rappresenta il numero di giorni tra le due date).

## Deep Dive

In PHP, le date possono essere rappresentate in diversi formati e possono anche contenere un'indicazione di fuso orario. Ciò potrebbe influire sui risultati quando si confrontano due date. Ad esempio, le date rappresentate in formato ISO 8601 (come nel nostro primo esempio) vengono considerate nel fuso orario UTC, mentre le date senza indicazione del fuso orario vengono considerate nel fuso orario del server.

Inoltre, quando si utilizza la funzione `strtotime()`, c'è un limite nel range di date supportate. Questo limite può variare a seconda del sistema operativo e dell'ambiente di PHP in cui viene eseguito.

Inoltre, quando si confrontano date che includono anche l'ora, è importante assicurarsi che i fusi orari siano consistenti per ottenere risultati accurati.

## Vedi anche

- [Documentazione PHP sulle funzioni di gestione delle date](https://www.php.net/manual/it/book.datetime.php)
- [Strumento di conversione di timestamp in data e viceversa](https://www.unixtimestamp.com/)