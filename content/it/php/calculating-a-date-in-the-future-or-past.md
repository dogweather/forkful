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

## Cos'è e Perché?

Il calcolo di una data futura o passata è un metodo per determinare una data che è un certo numero di giorni, settimane, mesi o anni avanti o indietro rispetto a una data specifica. I programmatori lo fanno per una serie di motivi, come programmare eventi ricorrenti o calcolare scadenze.

## Come fare:

Diamo un'occhiata al metodo `modify` della classe `DateTime` in PHP per calcolare una data futura. Di seguente è un semplice esempio di come lo faremmo:

```PHP
<?php
$date = new DateTime('2022-04-01');
$date->modify('+1 month');
echo $date->format('Y-m-d');
?>
```

In output avremo "2022-05-01", ovvero un mese dopo la data originale.

Se, invece, voleste calcolare una data nel passato, potete farlo in questo modo:

```PHP
<?php
$date = new DateTime('2022-04-01');
$date->modify('-1 year');
echo $date->format('Y-m-d');
?>
```

Questa volta avrete in output "2021-04-01", che è un anno prima della data originale.

## Approfondimenti

Il calcolo della data è uno dei problemi più antichi che gli informatici hanno cercato di risolvere. Da quando le prime macchine calcolatrici hanno iniziato a calcolare le date, gli sviluppatori hanno cercato modi per farlo in modo più efficiente e preciso.

Un'alternativa alla classe `DateTime` di PHP è la funzione `strtotime`. `strtotime` è una funzione potente che può convertire qualsiasi stringa di testo contenente una data in un timestamp Unix, che può quindi essere utilizzato per calcolare date future o passate. Tuttavia, `strtotime` può essere difficile da usare con formati di data non standard.

In termini di dettagli implementativi, è importante ricordare che il metodo `modify` modifica l'oggetto DateTime originale. Se non si desidera modificare l'oggetto originale, si dovrebbe clonare l'oggetto prima di chiamare `modify`.

## Per Saperne di Più

- Documentazione di PHP per la classe DateTime: [https://www.php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Documentazione PHP per la funzione strtotime: [https://www.php.net/manual/en/function.strtotime.php](https://www.php.net/manual/en/function.strtotime.php)
- Approccio alternativo al calcolo delle date con PHP: [https://stackoverflow.com/questions/676824/how-to-calculate-the-difference-in-days-between-two-calendar-dates](https://stackoverflow.com/questions/676824/how-to-calculate-the-difference-in-days-between-two-calendar-dates)
- Tutorial su Data e Ora in PHP: [https://www.w3schools.com/php/php_date.asp](https://www.w3schools.com/php/php_date.asp)