---
title:    "PHP: Converting una data in una stringa"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa è un'operazione fondamentale in qualsiasi progetto di programmazione PHP. Ciò consente di formattare correttamente la data per mostrarla all'utente o salvarla in un database. In questo articolo, parleremo di come eseguire questa operazione in modo efficace.

## Come fare

Per convertire una data in una stringa, è possibile utilizzare le funzioni date() o strftime() in PHP. Ad esempio, se vogliamo convertire la data di oggi nel formato DD-MM-YYYY, possiamo utilizzare il seguente codice:

```PHP
<?php
$date = date("d-m-Y");
echo "Oggi è il $date"; 
```

La funzione date() accetta due parametri: il formato della data desiderato e la data stessa. In questo caso, stiamo passando il formato "d-m-Y" per ottenere la data nel formato giorno-mese-anno.

Invece, se vogliamo formattare la data in base alla lingua o alle preferenze locali dell'utente, possiamo utilizzare la funzione strftime(). Ad esempio, per ottenere la data di oggi nel formato "Il giorno DD di MMMM YYYY" in italiano, possiamo utilizzare il seguente codice:

```PHP
<?php
$date = strftime("%A %e di %B del %Y");
echo "Oggi è $date"; 
```

La funzione strftime() accetta un parametro che definisce il formato di data desiderato. In questo caso, stiamo utilizzando i segnaposto (come %A per il nome del giorno e %B per il nome del mese) per ottenere il formato desiderato.

## Approfondimento

Per una conversione completa e precisa delle date in PHP, è importante comprendere i diversi formati di data e i segnaposto disponibili nelle funzioni date() e strftime(). Inoltre, è possibile utilizzare le funzioni di formattazione della classe DateTime per gestire le operazioni di conversione delle date in modo più avanzato.

Una corretta gestione delle date è essenziale per garantire che il proprio codice funzioni correttamente e sia compatibile con le varie lingue e preferenze dei propri utenti.

## Vedi anche

- [Documentazione ufficiale di PHP sulla funzione date()](http://php.net/manual/en/function.date.php)
- [Documentazione ufficiale di PHP sulla funzione strftime()](http://php.net/manual/en/function.strftime.php)
- [Documentazione ufficiale di PHP sulla classe DateTime](http://php.net/manual/en/class.datetime.php)