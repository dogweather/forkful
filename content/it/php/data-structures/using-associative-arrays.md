---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:37.804080-07:00
description: "Come fare: In PHP, creare e utilizzare array associativi \xE8 semplice.\
  \ Ecco una breve panoramica."
lastmod: '2024-03-13T22:44:43.509785-06:00'
model: gpt-4-0125-preview
summary: "In PHP, creare e utilizzare array associativi \xE8 semplice."
title: Utilizzo di array associativi
weight: 15
---

## Come fare:
In PHP, creare e utilizzare array associativi è semplice. Ecco una breve panoramica:

```PHP
<?php
// Creare un array associativo
$persona = array(
    "nome" => "John Doe",
    "età" => 30,
    "email" => "john@example.com"
);

// In alternativa, la sintassi breve dell'array
$persona = [
    "nome" => "John Doe",
    "età" => 30,
    "email" => "john@example.com"
];

// Accedere ai valori utilizzando le chiavi
echo "Nome: " . $persona["nome"] . "\n";
echo "Età: " . $persona["età"] . "\n";
echo "Email: " . $persona["email"] . "\n";

// Modificare un valore
$persona["età"] = 31;

// Aggiungere una nuova coppia chiave-valore
$persona["paese"] = "USA";

// Iterare su un array associativo
foreach ($persona as $chiave => $valore) {
    echo $chiave . ": " . $valore . "\n";
}

// Output
// Nome: John Doe
// Età: 31
// Email: john@example.com
// paese: USA
?>
```

Si noti come le chiavi possano essere qualsiasi stringa, permettendoti di accedere agli elementi utilizzando queste chiavi piuttosto che indici numerici, che possono essere meno significativi e più difficili da ricordare.

## Approfondimento
Gli array associativi in PHP sono implementati internamente tramite tabelle hash che forniscono un accesso molto rapido agli elementi per chiave, rendendoli molto efficienti per molti compiti. Questa efficienza, unita alla loro facilità di uso, rende gli array associativi una pietra miliare della programmazione PHP.

Storicamente, gli array di PHP (sia indicizzati che associativi) sono stati incredibilmente flessibili, permettendo loro di fungere da liste, pile, code e altro. Tuttavia, questa flessibilità può talvolta portare a codice meno efficiente se non utilizzata con attenzione.

Recentemente, con i miglioramenti nella programmazione orientata agli oggetti in PHP, alcuni sviluppatori preferiscono utilizzare oggetti per dati strutturati, in particolare per set di dati complessi o interrelati. L'uso delle classi può offrire una migliore incapsulamento e astrazione, rendere il codice più facile da testare e chiarire le intenzioni. Tuttavia, per semplici scenari di memorizzazione chiave-valore e manipolazione diretta dei dati, gli array associativi rimangono un'eccellente scelta a causa della loro semplicità e della sintassi intuitiva.
