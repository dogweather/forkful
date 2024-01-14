---
title:                "PHP: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-json.md"
---

{{< edit_this_page >}}

## Perché lavorare con JSON?

JSON (JavaScript Object Notation) è un formato di dati leggero e facile da leggere e scrivere. Viene comunemente utilizzato per lo scambio di dati tra applicazioni web e server. Con il crescente utilizzo di tecnologie come AJAX e REST API, la conoscenza di JSON è diventata una competenza fondamentale per i programmatori PHP.

## Come fare

Per utilizzare JSON con PHP, è necessario seguire questi passaggi:

1. Creare un array di dati in PHP.
2. Utilizzare la funzione `json_encode()` per convertire l'array in formato JSON.
3. Utilizzare la funzione `file_put_contents()` per scrivere il contenuto JSON su un file.
4. Per leggere i dati JSON, si può utilizzare la funzione `file_get_contents()` per ottenere il contenuto del file e quindi la funzione `json_decode()` per convertirlo in un array PHP.

Ecco un esempio di codice che mostra come creare un file JSON e leggerne i dati:

```PHP
<?php 
$data = array(
    "nome" => "Mario",
    "cognome" => "Rossi",
    "eta" => 30,
);
// Codifica l'array in formato JSON
$json_data = json_encode($data);

// Scrive i dati JSON su un file
file_put_contents('dati.json', $json_data);

// Legge i dati JSON dal file
$json_string = file_get_contents('dati.json');

// Decodifica il JSON in un array PHP
$php_data = json_decode($json_string, true);

// Output
echo $php_data["nome"]; // Mario
echo $php_data["cognome"]; // Rossi
echo $php_data["eta"]; // 30
```

## Approfondimento

Ci sono diverse funzioni di PHP che vengono utilizzate per lavorare con JSON:

- `json_encode()` per convertire un array PHP in JSON.
- `json_decode()` per convertire una stringa JSON in un array PHP.
- `json_last_error()` per ottenere l'ultimo errore verificatosi durante la conversione.
- `json_last_error_msg()` per ottenere una descrizione dell'ultimo errore verificatosi durante la conversione.
- `json_encode_options()` e `json_decode_options()` per specificare opzioni aggiuntive durante la conversione, come per esempio la formattazione del JSON.

Per ulteriori informazioni su queste funzioni e su come utilizzare JSON con PHP, si consiglia di consultare la documentazione ufficiale di PHP: https://www.php.net/manual/en/book.json.php

## Vedi anche

- [Introduction to JSON for Developers](https://www.json.org/json-it.html)
- [PHP JSON Functions](https://www.php.net/manual/en/ref.json.php)
- [AJAX introduction](https://www.w3schools.com/js/js_ajax_intro.asp)