---
title:                "PHP: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché convertire una data in una stringa?

In programmazione, ci sono molti casi in cui è necessario manipolare le date. A volte, potrebbe essere necessario convertire una data in una stringa per poterla visualizzare correttamente o utilizzarla in un'operazione di confronto. In questo articolo, vedremo come fare questa conversione utilizzando PHP e quali sono le migliori pratiche da seguire.

## Come convertire una data in una stringa

Per convertire una data in una stringa, possiamo utilizzare la funzione `date` di PHP. Questa funzione accetta due parametri: il formato desiderato per la data e la data da convertire. Ad esempio, se vogliamo visualizzare la data di oggi nel formato "giorno, mese anno", possiamo utilizzare il seguente codice:

```PHP
$data = date("d, m Y", time());
echo $data; // Output: 27, 11 2020
```

È importante notare che il parametro `time()` viene utilizzato per specificare la data di oggi. Tuttavia, possiamo passare qualsiasi altra data come secondo parametro se vogliamo convertire una data specifica invece della data corrente.

## Deep Dive

La funzione `date` di PHP supporta una vasta gamma di formati per le date. Ad esempio, possiamo utilizzare i seguenti caratteri per formattare la data:

- `d`: giorno del mese, con due cifre
- `m`: mese, con due cifre
- `Y`: anno con quattro cifre
- `D`: abbreviazione del giorno della settimana (es. Mon, Tue, Wed...)
- `M`: abbreviazione del mese (es. Jan, Feb, Mar...)
- `l`: nome completo del giorno della settimana (es. Monday, Tuesday, Wednesday...)
- `F`: nome completo del mese (es. January, February, March...)

Inoltre, possiamo anche utilizzare caratteri speciali come `/`, `.` o `-` per separare i parametri del formato utilizzati. Ad esempio:

```PHP
$data = date("d-m-Y", time());
echo $data; // Output: 27-11-2020

$data = date("l, d M Y", time());
echo $data; // Output: Friday, 27 Nov 2020
```

## Vedi anche

- [PHP date function documentation](https://www.php.net/manual/en/function.date.php)
- [10 common date formatting mistakes in PHP](https://www.noupe.com/php/common-php-date-format-mistakes.html)
- [How to compare dates in PHP](https://www.php.net/manual/en/datetime.diff.php)