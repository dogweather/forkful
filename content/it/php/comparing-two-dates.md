---
title:    "PHP: Confrontare due date"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'operazione comune nell'ambito della programmazione e può essere utile in molte situazioni, come ad esempio per confrontare date di nascita o date di scadenza di prodotti. Grazie alla flessibilità del linguaggio di programmazione PHP, è possibile eseguire questo tipo di confronto in pochi passi semplici e veloci.

## Come fare

Per confrontare due date in PHP, è necessario utilizzare la funzione built-in `strtotime()`. Questa funzione converte una stringa di testo in una data nel formato UNIX timestamp, che rappresenta il numero di secondi trascorsi dal 1° gennaio 1970 alle 00:00:00 UTC. Ecco un esempio di utilizzo:

```PHP
$date1 = "2020-05-20"; //prima data
$date2 = "2020-06-15"; //seconda data

$timestamp1 = strtotime($date1); //converte la prima data in timestamp
$timestamp2 = strtotime($date2); //converte la seconda data in timestamp

if ($timestamp1 < $timestamp2) {
  echo $date1 . " è precedente a " . $date2;
} elseif ($timestamp1 > $timestamp2) {
  echo $date1 . " è successiva a " . $date2;
} else {
  echo $date1 . " è uguale a " . $date2;
}

// Output: 2020-05-20 è precedente a 2020-06-15
```

In questo esempio, le due date vengono convertite in timestamp e poi confrontate utilizzando il costrutto `if-elseif-else`. A seconda del risultato, viene stampato un messaggio appropriato.

È importante notare che la funzione `strtotime()` accetta una vasta gamma di formati di data, quindi è possibile utilizzare questo metodo per confrontare date in diversi formati.

## Approfondimento

Esistono anche altre funzioni utili per il confronto di date in PHP, come ad esempio `date_diff()`, che calcola la differenza tra due date e restituisce un oggetto `DateInterval`, contenente informazioni sulla differenza in giorni, ore, minuti e secondi. Inoltre, è possibile utilizzare le funzioni `date_create()` e `date_format()` per creare oggetti di tipo `DateTime` e formattare una data in un formato specifico.

Per una guida completa alle funzioni per la gestione delle date in PHP, si consiglia di consultare la documentazione ufficiale [qui](https://www.php.net/manual/en/book.datetime.php).

## Vedi anche

- [Documentazione ufficiale di PHP sulle funzioni per la gestione delle date](https://www.php.net/manual/en/book.datetime.php)
- [Articolo sul confronto di date in PHP](https://www.phptutorial.net/php-tutorial/php-date-comparison/)