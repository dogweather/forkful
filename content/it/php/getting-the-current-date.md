---
title:                "Ottenere la data corrente"
html_title:           "PHP: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & perché?

Ottenere la data corrente è una funzione importante in ogni programma, in quanto fornisce informazioni necessarie per la gestione del tempo. Ad esempio, potrebbe essere utilizzata per tenere traccia dei record delle attività o per impostare scadenze. I programmatori fanno uso di essa per garantire che il loro software sia in grado di gestire il tempo in modo accurato ed efficiente.

## Come fare:

```PHP
$date = date("d-m-Y"); //ottieni la data corrente nel formato: giorno-mese-anno
echo $date; //stampa la data corrente
```

Output: 02-07-2021

## Approfondimento:

### Contesto storico:
Ottenere la data corrente è diventato essenziale con l'avvento dei computer e dei sistemi informatici, in quanto ha permesso di automatizzare numerosi processi legati alla gestione del tempo. In passato, era necessario utilizzare un calendario o un'agenda per tenere traccia delle attività, ma ora è possibile ottenere la data e l'ora esatta in pochi secondi.

### Alternative:
Oltre alla funzione `date()` di PHP, è possibile utilizzare altri metodi per ottenere la data corrente, come l'utilizzo di API esterne o di librerie specifiche. Tuttavia, la funzione `date()` è la soluzione più semplice ed efficace per la maggior parte dei casi.

### Dettagli implementativi:
In PHP, il formato della data è determinato da una stringa di formattazione, come visto nell'esempio precedente (`d-m-Y`). È possibile utilizzare diversi parametri per personalizzare la visualizzazione della data, come ad esempio `m` per il mese in numeri o `F` per il mese in lettere. È importante fare riferimento alla documentazione ufficiale di PHP per selezionare il formato corretto per le proprie esigenze.

## Vedi anche:

- [PHP date() Function - W3Schools](https://www.w3schools.com/php/func_date_date.asp)
- [PHP: date - Manuale PHP](https://www.php.net/manual/it/function.date.php)