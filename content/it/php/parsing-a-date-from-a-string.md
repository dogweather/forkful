---
title:                "Analisi di una data da una stringa."
html_title:           "PHP: Analisi di una data da una stringa."
simple_title:         "Analisi di una data da una stringa."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Il parsing di una data da una stringa è il processo di estrarre una data da una stringa di testo. I programmatori spesso lo utilizzano quando devono manipolare le date in un formato diverso da quello in cui sono memorizzate, per esempio per visualizzarle in un formato più leggibile per gli utenti.

## Come fare:
Ecco un esempio di codice PHP che utilizza le funzioni di parsing della data per estrarre la data in formato italiano e visualizzarla come stringa:

```PHP
$date_str = '15/02/2021';
$date = date_parse_from_format('d/m/Y', $date_str); 

// visualizza la data come stringa nel formato "DD/MM/YYYY"
echo "{$date['day']}/{$date['month']}/{$date['year']}";
```

**Output:** 15/02/2021

## Approfondimento:
Il parsing delle date da una stringa è diventato un processo necessario con l'avvento dei cosiddetti *Unix timestamps*, ovvero la rappresentazione delle date come numero di secondi trascorsi dal 1 gennaio 1970. Alcune alternative per eseguire il parsing di una data da una stringa sono l'utilizzo di espressioni regolari o di librerie esterne come Carbon. Il PHP offre varie funzioni di parsing della data, come `date_parse` e `strtotime`, che possono essere utilizzate per ottenere diverse informazioni dalla data, come il numero di settimane trascorse o il giorno della settimana.

## Vedi anche:
- [Documentazione ufficiale delle funzioni di data e ora in PHP](https://www.php.net/manual/en/function.date.php)
- [Libreria Carbon per il parsing e la manipolazione delle date in PHP](https://carbon.nesbot.com/)