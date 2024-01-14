---
title:                "PHP: Ottenere la data corrente."
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Oggi parleremo di una funzione importante nella programmazione PHP, ovvero la possibilità di ottenere la data corrente. Questo può sembrare semplice, ma in realtà è un'operazione fondamentale per molti progetti. Vediamo perché.

## Come ottenere la data corrente in PHP

Per ottenere la data corrente in PHP possiamo utilizzare la funzione `date()`. Questa funzione ha come parametro una stringa che contiene il formato della data che vogliamo visualizzare. Di seguito un esempio di codice:

```PHP
$data_corrente = date('d/m/Y');
echo $data_corrente;
```

Questo codice produrrà un output simile a `09/07/2021`, dove la prima parte rappresenta il giorno, la seconda il mese e la terza l'anno. 

Invece, se volessimo includere anche l'ora, possiamo utilizzare il parametro `H:i:s`. Esempio:

```PHP
$data_ora_corrente = date('d/m/Y H:i:s');
echo $data_ora_corrente;
```

Questo produrrà un output del tipo `09/07/2021 10:30:15`, includendo anche l'ora, i minuti e i secondi. È importante notare che questa funzione restituisce la data e l'ora correnti del server in cui il codice viene eseguito.

## Approfondimento

La funzione `date()` utilizza una stringa di formato per determinare come visualizzare la data. Alcuni dei parametri utilizzabili sono:

- `d`: giorno con due cifre (es. 09)
- `m`: mese con due cifre (es. 07)
- `Y`: anno con quattro cifre (es. 2021)
- `H`: ora in formato 24 ore (es. 10)
- `i`: minuti (es. 30)
- `s`: secondi (es. 15)
- `l`: nome del giorno della settimana (es. venerdì)
- `F`: nome del mese (es. luglio)

Per una lista completa dei parametri disponibili, è possibile consultare la documentazione ufficiale di PHP.

## Vedi anche

- [Funzione date() su PHP.net](https://www.php.net/manual/en/function.date.php)
- [Documentazione ufficiale di PHP](https://www.php.net/manual/en/)

Grazie per aver letto questo articolo, speriamo ti sia stato utile per comprendere meglio come ottenere la data corrente in PHP. Alla prossima!