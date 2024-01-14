---
title:    "Gleam: Convertire una data in una stringa."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa può sembrare un'operazione semplice, ma è fondamentale per la gestione e la visualizzazione delle informazioni temporali nei tuoi programmi Gleam. In questo articolo, ti mostreremo come farlo in modo efficiente utilizzando alcune funzioni built-in di Gleam.

## Come fare

Per prima cosa, è necessario specificare il formato della stringa di output in cui desideri visualizzare la data. Ad esempio, se vuoi esprimere la data nel formato "giorno mese anno", userai il codice "%d %B %Y". Di seguito è riportato un esempio di come creare una stringa formattata utilizzando la funzione strftime:

```Gleam
let data = Time.now()
let stringa = Time.strftime(data, "%d %B %Y")
```

Il valore della stringa sarà ora "21 maggio 2021".

Inoltre, puoi specificare il fuso orario desiderato per la tua data utilizzando la funzione timezone(), che prende come argomento una stringa contenente il nome del fuso orario. Ad esempio, se vuoi visualizzare la data nel fuso orario italiano, utilizzerai il codice "Europe/Rome". Di seguito è riportato un esempio di come utilizzare questa funzione:

```Gleam
let data = Time.timezone("Europe/Rome")
```

Questo ti restituirà la data corrente nel fuso orario specificato.

## Approfondimenti

Oltre alle funzioni menzionate sopra, esistono altre opzioni per la formattazione delle date in stringhe, come ad esempio l'utilizzo delle variabili Time.second, Time.minute, Time.hour, ecc. Inoltre, esistono anche altre funzioni utili per la gestione dei tempi, come Time.add() e Time.diff(), che possono essere utilizzate per eseguire operazioni aritmetiche con le date.

Inoltre, è importante tenere in considerazione il formato delle date utilizzato dal sistema operativo su cui viene eseguito il tuo programma Gleam. Assicurati sempre di adattare il codice di conseguenza per evitare errori di conversione.

See Also:

- Documentazione Time module di Gleam: https://gleam.run/modules/time.html
- Tutorial su come usare il modulo Time di Gleam: https://gleam.run/tutorials/time.html