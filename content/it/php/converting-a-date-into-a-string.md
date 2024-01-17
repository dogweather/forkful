---
title:                "La conversione di una data in una stringa"
html_title:           "PHP: La conversione di una data in una stringa"
simple_title:         "La conversione di una data in una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La conversione di una data in una stringa è un'operazione comune nella programmazione. Consiste nel trasformare una data, rappresentata usualmente in forma numerica, in una stringa di caratteri che ne descrive il valore in modo leggibile per l'utente. I programmatori spesso eseguono questa operazione per visualizzare date su pagine web o in formati specifici.

## Come fare:

```PHP
// Esempio 1: Utilizzando la funzione date()
$oggi = date('d-m-Y'); // 19-04-2021
```
```PHP
// Esempio 2: Utilizzando la classe DateTime
$data = new DateTime('2021-04-19');
echo $data->format('d-m-Y'); // 19-04-2021
```
In entrambi gli esempi, abbiamo convertito la data corrente nel formato "giorno-mese-anno" e l'abbiamo assegnata a una variabile. Questo ci permette di stampare la data in modo leggibile all'utente.

## Approfondimento:

La conversione di una data in una stringa è diventata fondamentale con l'avvento di internet e della programmazione web. Originariamente, le date venivano rappresentate in formato numerico, ma la necessità di visualizzare date in modo leggibile ha portato alla creazione di funzioni e classi apposite per la conversione. Oltre alle funzioni date() e alla classe DateTime, esistono altri metodi per convertire le date, come ad esempio la funzione strftime() che permette di creare formati personalizzati basati su specifiche regole.

## Vedi anche:

- [Documentazione PHP sulle funzioni date](https://www.php.net/manual/en/function.date.php)
- [Esempi di formati di date e ora](https://www.php.net/manual/en/datetime.format.php)
- [Tutorial su come utilizzare la classe DateTime](https://www.php.net/manual/en/class.datetime.php)