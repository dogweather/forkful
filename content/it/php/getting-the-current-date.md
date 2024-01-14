---
title:    "PHP: Ottenere la data attuale"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data è un elemento fondamentale in quasi ogni applicazione. Con il PHP, è possibile ottenere facilmente la data corrente utilizzando alcune semplici funzioni.

## Come fare

Per ottenere la data corrente con PHP, è possibile utilizzare la funzione `date()`. Questa funzione accetta due parametri: il formato della data desiderato e la marca temporale opzionale. Ecco un esempio di come utilizzare questa funzione per ottenere la data odierna:

```
<?php
$data_corrente = date("d/m/Y");
echo "Oggi è il $data_corrente";
```

Il codice sopra dovrebbe produrre un output simile a questo:

```
Oggi è il 08/09/2021
```

Se si vuole ottenere la data corrente con la marca temporale, è possibile utilizzare la funzione `time()` insieme alla funzione `date()`, come mostrato nell'esempio seguente:

```
<?php
$marca_temporale = time();
$data_corrente = date("d/m/Y", $marca_temporale);
echo "La data corrente in marca temporale è $marca_temporale e nel formato desiderato è $data_corrente";
```

L'output dovrebbe essere simile a questo:

```
La data corrente in marca temporale è 1631088686 e nel formato desiderato è 08/09/2021
```

## Approfondimento

Se si vuole saperne di più su come funzionano le funzioni `date()` e `time()` in PHP, è possibile approfondire la documentazione ufficiale sul sito di PHP. Inoltre, esistono altre funzioni che permettono di manipolare le date e il tempo in PHP, come ad esempio `strtotime()` per convertire una stringa in una marca temporale.

## Vedi anche
- [Documentazione ufficiale su date e tempo in PHP](https://www.php.net/manual/en/function.date.php)
- [Funzione strtotime() in PHP](https://www.php.net/manual/en/function.strtotime.php)
- [Altri esempi e approfondimenti su date e tempo in PHP](https://www.w3schools.com/php/php_dates.asp)