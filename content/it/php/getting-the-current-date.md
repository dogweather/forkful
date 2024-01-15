---
title:                "Ottenere la data corrente."
html_title:           "PHP: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è una parte fondamentale di qualsiasi applicazione che utilizza il concetto di tempo. Ad esempio, può essere utilizzata per registrare la data in cui un utente si è registrato o per calcolare scadenze. Con il PHP, è possibile ottenere la data corrente in modo semplice e veloce.

## Come fare

Per ottenere la data corrente in PHP, è necessario utilizzare la funzione `date()`. Questa funzione accetta due parametri: il primo è il formato della data desiderato e il secondo è un timestamp opzionale, che indica il momento specifico di cui si vuole ottenere la data. Di seguito un esempio di come utilizzare questa funzione per ottenere la data corrente nel formato "giorno/mese/anno":

```PHP
<?php
  $data = date('d/m/Y');
  echo $data;
?>
```

L'output di questo codice sarà qualcosa del tipo "20/05/2021", a seconda della data in cui viene eseguito. Se si desidera ottenere la data corrente nel formato "giorno della settimana, giorno mese anno", è possibile utilizzare il seguente codice:

```PHP
<?php
  $data = date('l, d F Y');
  echo $data;
?>
```

L'output di questo codice sarà qualcosa del tipo "giovedì, 20 maggio 2021".

Se si vuole ottenere la data corrente in un fuso orario diverso da quello predefinito del server, è possibile utilizzare la funzione `date_default_timezone_set()`. Ad esempio, se si vuole ottenere la data corrente nel fuso orario di Roma, è possibile utilizzare il seguente codice:

```PHP
<?php
  date_default_timezone_set('Europe/Rome');
  $data = date('d/m/Y H:i:s');
  echo $data;
?>
```

L'output di questo codice sarà qualcosa del tipo "20/05/2021 13:00:00".

## Approfondimento

Oltre alla funzione `date()`, PHP mette a disposizione altre funzioni per gestire la data e l'orario. Ad esempio, la funzione `time()` restituisce il numero di secondi trascorsi dal 1° gennaio 1970 a mezzanotte nell'ora locale. Questo valore può essere utilizzato come timestamp nella funzione `date()` per ottenere la data in un momento specifico. Inoltre, è disponibile anche la funzione `strtotime()`, che converte una stringa di testo nel formato di data in un timestamp.

## Vedi anche

- Funzione `date()` nella documentazione ufficiale di PHP: https://www.php.net/manual/en/function.date.php
- Funzione `time()` nella documentazione ufficiale di PHP: https://www.php.net/manual/en/function.time.php
- Funzione `strtotime()` nella documentazione ufficiale di PHP: https://www.php.net/manual/en/function.strtotime.php