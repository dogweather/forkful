---
date: 2024-01-20 17:35:25.977007-07:00
description: "Concatenare stringhe significa unire due o pi\xF9 testi in uno. I programmatori\
  \ lo fanno per costruire dinamicamente messaggi, SQL query, URL, ecc."
lastmod: '2024-03-13T22:44:43.508862-06:00'
model: gpt-4-1106-preview
summary: "Concatenare stringhe significa unire due o pi\xF9 testi in uno. I programmatori\
  \ lo fanno per costruire dinamicamente messaggi, SQL query, URL, ecc."
title: Concatenazione di stringhe
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Concatenare stringhe significa unire due o più testi in uno. I programmatori lo fanno per costruire dinamicamente messaggi, SQL query, URL, ecc.

## How to (Come Fare)
```PHP
// Concatenazione con l'operatore punto
$testo1 = 'Ciao ';
$testo2 = 'Mondo!';
$unione = $testo1 . $testo2;
echo $unione; // Ciao Mondo!

// Concatenazione con le virgolette doppie
$variabile = 'pianeta';
echo "Benvenuto sul $variabile!"; // Benvenuto sul pianeta!

// Unire più stringhe
$uno = 'PHP ';
$due = 'è ';
$tre = 'fantastico!';
$frase = $uno . $due . $tre;
echo $frase; // PHP è fantastico!
```

## Deep Dive (Approfondimento)
La concatenazione di stringhe in PHP esiste da quando è stato creato il linguaggio nel 1995. Alternativamente, si può utilizzare l'interpunzione delle variabili all'interno di stringhe racchiuse in virgolette doppie, ma è consigliabile per variabili semplici e non per espressioni complesse.

La nuova versione di PHP (dalla 8.0) ha introdotto l'operatore di assegnazione di concatenazione (`.=`), che semplifica l'aggiunta di una stringa a un'altra esistente.

```PHP
$inizio = 'PHP ';
$inizio .= 'è flessibile!';
echo $inizio; // PHP è flessibile!
```

Altre lingue hanno diversi operatori o metodi di concatenazione (come `+` in JavaScript o `.join()` in Python), ma il classico operatore punto di PHP resta un metodo diretto e facilmente comprensibile.
