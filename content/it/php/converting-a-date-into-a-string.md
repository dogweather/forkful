---
title:                "PHP: Convertire una data in una stringa"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa può sembrare un compito semplice, ma è fondamentale per qualsiasi programma che gestisca date e orari. Con l'aiuto di PHP, è possibile convertire facilmente una data in un formato leggibile e comprensibile per gli utenti.

## Come Fare

Per convertire una data in una stringa, è necessario utilizzare la funzione `date()` di PHP. Questa funzione accetta due parametri: il primo è il formato della stringa di output e il secondo è la data da convertire. Ad esempio, se si vuole convertire la data odierna in un formato leggibile, si può utilizzare il seguente codice:

```PHP
date("d-m-Y", time());
```

Questo produrrà un'output simile a questo: 01-01-2021. È possibile personalizzare il formato della stringa di output utilizzando le varie lettere e simboli disponibili nella documentazione di PHP. Ad esempio, `d` rappresenta il giorno del mese, `m` rappresenta il mese e `Y` rappresenta l'anno. È anche possibile aggiungere testo aggiuntivo nella stringa utilizzando le virgolette.

## Approfondimenti

È importante tenere conto del fuso orario e delle impostazioni regionali quando si converte una data in una stringa. Per impostazione predefinita, PHP utilizza il fuso orario del server, ma è possibile impostarne uno personalizzato utilizzando la funzione `date_default_timezone_set()`.

Inoltre, è possibile utilizzare la funzione `strtotime()` per convertire una stringa in una data. Questo può essere utile se si desidera convertire una data specifica in un formato diverso da quello predefinito.

## Vedi Anche

Per ulteriori informazioni su come gestire le date con PHP, si consiglia di consultare la documentazione ufficiale: 

- [Funzione `date()` di PHP](https://www.php.net/manual/en/function.date.php)
- [Funzione `date_default_timezone_set()` di PHP](https://www.php.net/manual/en/function.date-default-timezone-set.php)
- [Funzione `strtotime()` di PHP](https://www.php.net/manual/en/function.strtotime.php)