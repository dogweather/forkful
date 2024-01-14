---
title:                "PHP: Calcolare una data nel futuro o nel passato"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Il calcolo di una data futura o passata è una skill molto utile per i programmatori PHP, poiché può essere utilizzata per una varietà di scopi, come ad esempio programmare task ricorrenti o visualizzare le date in modo personalizzato.

## Come Fare
Per calcolare una data futura o passata in PHP, è necessario utilizzare la funzione "strtotime", che accetta come parametro una stringa rappresentante la data e restituisce un timestamp. Ecco un esempio di codice:

```PHP
<?php
$oggi = time(); //timestamp per la data odierna
$data_futura = strtotime("+1 week", $oggi); //calcola la data di una settimana nel futuro
echo date("d-m-Y", $data_futura); //stampa la data in formato gg-mm-aaaa
```

L'output di questo codice sarà "03-09-2020", poiché il timestamp restituito è relativo alla data odierna.

## Deep Dive
La funzione "strtotime" accetta una vasta gamma di input che permettono di specificare date future o passate in modo preciso. Ad esempio, è possibile utilizzare espressioni come "next week" o "last Monday" per calcolare una data relativa alla prossima o ultima settimana. Inoltre, è possibile utilizzare timestamp personalizzati come secondo parametro per ottenere una data basata su un punto di riferimento specifico.

## Vedi Anche
- [Documentazione ufficiale di PHP su strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [Esempi pratici di calcolo di date in PHP](https://www.php.net/manual/en/datetime.examples.php)