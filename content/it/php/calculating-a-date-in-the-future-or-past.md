---
title:                "PHP: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare la data in futuro o nel passato è un'operazione spesso necessaria durante la programmazione. Potresti voler visualizzare eventi futuri in un calendario o avere bisogno di calcolare la scadenza di un certificato o di un contratto. In questa guida vedremo come eseguire questa operazione utilizzando PHP.

## Come fare

Per calcolare una data in futuro o nel passato, possiamo utilizzare la funzione `strtotime` di PHP. Questa funzione accetta due parametri: una stringa che rappresenta una data e un parametro opzionale che definisce la data di base per il calcolo. Ad esempio, se vogliamo calcolare la data 2 settimane nel futuro, possiamo utilizzare la seguente sintassi:

```
<?php
$date = strtotime("+2 weeks");
echo date("d/m/Y", $date);
```

Questo codice eseguirà il calcolo e ci restituirà la data corrispondente a 2 settimane a partire dalla data corrente, formattata nel formato "giorno/mese/anno". Possiamo anche specificare una data di base per il calcolo come secondo parametro della funzione `strtotime`, ad esempio:

```
<?php
$date = strtotime("+2 weeks", strtotime("10 March 2020"));
echo date("d/m/Y", $date);
```

In questo caso, il calcolo verrà effettuato a partire dalla data "10 March 2020" anziché dalla data corrente.

## Approfondimenti

La funzione `strtotime` di PHP accetta una varietà di formati di input per rappresentare una data, come ad esempio "tomorrow" (domani), "next week" (la prossima settimana), "next month" (il prossimo mese), "next year" (l'anno prossimo), o anche date specifiche come "10 March 2020". Inoltre, possiamo utilizzare operatori matematici come "+", "-" e "*", ad esempio "+2 weeks" per aggiungere due settimane alla data di base. Per ulteriori informazioni su come utilizzare la funzione `strtotime`, consulta la [documentazione ufficiale di PHP](https://www.php.net/manual/en/function.strtotime.php).

## Vedi anche

- [Documentazione ufficiale di PHP su strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [Come calcolare una data di scadenza in PHP](https://www.php.net/manual/en/function.strtotime.php)
- [Calcolare il numero di giorni tra due date in PHP](https://www.php.net/manual/en/function.strtotime.php)