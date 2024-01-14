---
title:                "PHP: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Il concatenamento di stringhe è un'importante funzione di programmazione PHP che permette di unire più stringhe in una sola, creando un output personalizzato per il nostro codice. In questo post, vedremo come utilizzarla e approfondiremo in che modo ci può essere utile nella nostra programmazione.

## Come fare
Per concatenare due o più stringhe in PHP, possiamo utilizzare l'operatore "." seguito dalle stringhe che vogliamo unire. Ad esempio, se abbiamo due variabili $nome e $cognome, possiamo concatenarle in questo modo:

```PHP
$nome = "Maria";
$cognome = "Rossi";
echo $nome . $cognome;
```

Questo ci darà come output "MariaRossi". Possiamo inoltre aggiungere caratteri di spaziatura o di punteggiatura tra le stringhe concatenandoli direttamente all'operatore ".". Vediamo un esempio:

```PHP
$nome = "Maria";
$cognome = "Rossi";
echo $nome . " " . $cognome . ".";
```

In questo caso, l'output sarà "Maria Rossi." Possiamo anche concatenare stringhe con numeri o altre variabili in modo simile.

## Approfondimento
Il concatenamento di stringhe può sembrare una funzione molto semplice, ma è estremamente utile nella programmazione PHP. Ad esempio, possiamo utilizzarlo per creare URL dinamici o per generare testi personalizzati a seconda delle variabili presenti nel nostro codice. Ci sono molti casi in cui il concatenamento di stringhe ci permette di rendere più dinamico e performante il nostro codice.

## Vedi anche
- [La documentazione ufficiale di PHP sul concatenamento di stringhe](https://www.php.net/manual/it/language.operators.string.php)
- [Un tutorial dettagliato su come utilizzare il concatenamento di stringhe in PHP](https://www.html.it/pag/43865/il-concatenamento-di-stringhe-in-php/)
- [Un articolo sui vantaggi del concatenamento di stringhe nella programmazione orientata agli oggetti in PHP](https://www.tutorialspoint.com/php5/php5_object_oriented.htm)