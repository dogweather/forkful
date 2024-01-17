---
title:                "Confrontare due date"
html_title:           "PHP: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

### Cosa e perché?
Confrontare due date è un'operazione comune nella programmazione che consiste nel confrontare due date per valutare se una è successiva, precedente o uguale all'altra. Questo è spesso utile per gestire dati temporali come ad esempio le scadenze dei pagamenti o i post sui social media.

### Come fare:
Il modo più semplice per confrontare due date in PHP è utilizzare la funzione ```strcmp ()``` che confronta due stringhe e restituisce 0 se sono uguali, un numero positivo se la prima è maggiore della seconda e un numero negativo se è minore. Vediamo un esempio:

```PHP
$date1 = "2021-01-01";
$date2 = "2021-01-10";
$result = strcmp ($date1, $date2);
echo $result;
```
In questo caso, il risultato stampato sarà negativo poiché la prima data è precedente alla seconda.

Una seconda opzione è l'utilizzo della funzione ```strtotime ()``` che converte una data in un timestamp. Possiamo quindi confrontare due timestamp e ottenere un risultato simile a quello della funzione ```strcmp ()```. Esempio:

```PHP
$date1 = "2021-01-01";
$date2 = "2021-01-10";
$timestamp1 = strtotime ($date1);
$timestamp2 = strtotime ($date2);
$result = $timestamp1 - $timestamp2;
```
Anche in questo caso, il risultato sarà negativo poiché il timestamp della prima data è minore di quello della seconda.

### Approfondimento:
Questa operazione è possibile grazie all'introduzione della classe DateTime in PHP 5.2. Con questa classe, è possibile non solo confrontare due date, ma anche eseguire altre operazioni come ad esempio convertire date in diversi formati o aggiungere o sottrarre un intervallo di tempo. Inoltre, esistono anche altre funzioni utili come ```date_diff ()``` che restituisce la differenza tra due date in giorni, ore, minuti o secondi.

Come alternativa a PHP, è possibile utilizzare librerie esterne come Moment.js per gestire date e orari in modo più efficiente e preciso.

Per implementare il confronto di due date, è necessario assicurarsi che siano formattate correttamente e che siano dello stesso tipo. Ad esempio, se una delle due date è una stringa e l'altra è un oggetto DateTime, sarà necessario convertire entrambe in lo stesso tipo prima di eseguire la comparazione.

### Vedi anche:
- [Documentazione ufficiale di PHP sulla classe DateTime] (https://www.php.net/manual/it/class.datetime.php)
- [Moment.js] (https://momentjs.com/) - una popolare libreria per la gestione delle date in JavaScript